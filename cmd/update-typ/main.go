// update-typ rewrites Typst files to update dependencies to their latest
// version.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"github.com/mewmew/typast/typst/cstwalk"
	"github.com/mewmew/typast/typst/syntax"
	"github.com/mewpkg/clog"
	"github.com/pkg/errors"
)

func init() {
	clog.SetMainPrefixName("update-typ")
}

var (
	// inplace specifies whether to edit files in place.
	inplace bool
	// outDir specifies the output directory.
	outDir string
	// projectRootDir specifies the project root directory.
	projectRootDir string
	// universeDir specifies the Typst universe `packages` repo directory.
	//
	// https://github.com/typst/packages.git
	universeDir string
	// verbose specifies whether to use verbose debug output.
	verbose bool
)

func usage() {
	flag.PrintDefaults()
	fmt.Fprintln(os.Stderr, "Usage: update-typ [OPTION]... INPUT.typ")
}

func main() {
	// parse command line arguments.
	flag.Usage = usage
	flag.BoolVar(&verbose, "v", false, "verbose debug output")
	flag.BoolVar(&inplace, "i", false, "edit files in place")
	flag.StringVar(&outDir, "out", "out", "output directory")
	flag.StringVar(&projectRootDir, "root", "", "project root directory")
	flag.StringVar(&universeDir, "universe", "", "Typst universe 'packages' repo directory")
	flag.Parse()
	if flag.NArg() != 1 {
		flag.Usage()
		os.Exit(1)
	}
	typPath := flag.Arg(0)
	absTypPath, err := filepath.Abs(typPath)
	if err != nil {
		clog.Fatalf("unable to get absolute path of %q; %+v", typPath, errors.WithStack(err))
	}

	// open project root directory.
	if len(projectRootDir) > 0 {
		absProjectRootDir, err := filepath.Abs(projectRootDir)
		if err != nil {
			clog.Fatalf("unable to get absolute path of %q; %+v", projectRootDir, errors.WithStack(err))
		}
		projectRootDir = absProjectRootDir
		relTypPath, err := stripPrefix(absTypPath, absProjectRootDir)
		if err != nil {
			clog.Fatalf("unable to strip project root prefix from Typst file path; %+v", err)
		}
		typPath = relTypPath
	} else {
		dir, file := filepath.Split(absTypPath)
		projectRootDir = dir
		typPath = file // relative path
	}
	projectRoot, err := os.OpenRoot(projectRootDir)
	if err != nil {
		clog.Fatalf("unable to open project root directory; %+v", errors.WithStack(err))
	}
	if !verbose {
		clog.SetPathLevel("main", clog.LevelInfo)
		clog.SetPathLevel("github.com/mewmew/typast", clog.LevelInfo)
	}

	// open output root directory.
	outRoot := projectRoot // for in place edit (`-i` flag)
	if !inplace {
		if err := os.MkdirAll(outDir, 0o755); err != nil {
			clog.Fatalf("%+v", err)
		}
		outRoot, err = os.OpenRoot(outDir)
		if err != nil {
			clog.Fatalf("unable to open output root directory; %+v", errors.WithStack(err))
		}
	}

	// rewrite Typst files to update dependencies to their latest version.
	specs, err := rewrite(outRoot, projectRoot, typPath)
	if err != nil {
		clog.Fatalf("%+v", err)
	}

	// check for and report old transitive dependencies.
	if err := checkLibs(specs); err != nil {
		clog.Fatalf("%+v", err)
	}
	if inplace {
		clog.Infof("stored updated version in place")
	} else {
		clog.Infof("stored updated version in %q directory", outDir)
	}
}

var (
	// track rewritten Typst files.
	//
	// key: full path to Typst file.
	typDone = make(map[string]bool)
)

// rewrite rewrites the given Typst file to update dependencies to their latest
// version.
func rewrite(outRoot, projectRoot *os.Root, relTypPath string) ([]*syntax.PackageSpec, error) {
	// check if already processed.
	absTypPath := filepath.Join(projectRoot.Name(), relTypPath)
	if typDone[absTypPath] {
		clog.Debugf("skipping %q (already processed)", absTypPath)
		return nil, nil // already processed.
	}
	typDone[absTypPath] = true

	// create output directory if needed.
	clog.Debugf("rewriting %q", relTypPath)
	relTypDir := filepath.Dir(relTypPath)
	if err := outRoot.MkdirAll(relTypDir, 0o755); err != nil {
		return nil, errors.WithStack(err)
	}

	// parse Typst source.
	rootNode, err := parse(projectRoot, relTypPath)
	if err != nil {
		return nil, errors.WithStack(err)
	}

	// Check for old versions of dependencies.
	specs, localTypPaths := findOldVersions(rootNode, relTypPath, "")

	// Rewrite local Typst files.
	for _, localTypPath := range localTypPaths {
		localSpecs, err := rewrite(outRoot, projectRoot, localTypPath)
		if err != nil {
			return nil, errors.WithStack(err)
		}
		specs = append(specs, localSpecs...)
	}

	// output new version of file.
	out := &bytes.Buffer{}
	syntax.PrintNode(out, rootNode)
	if err := outRoot.WriteFile(relTypPath, out.Bytes(), 0o644); err != nil {
		return nil, errors.WithStack(err)
	}

	return specs, nil
}

// findOldVersions updates the import statements of dependencies used by the
// given CST to their latest version. It returns a list of package imports left
// to check for new version of dependencies and a list of local Typst files to
// process.
func findOldVersions(rootNode *syntax.SyntaxNode, relTypPath string, libName string) ([]*syntax.PackageSpec, []string) {
	relTypDir := filepath.Dir(relTypPath)
	var (
		// package imports to check for new versions of dependencies.
		specs []*syntax.PackageSpec
		// local Typst files to rewrite to update dependencies to their latest
		// version.
		localTypPaths []string
	)
	// walk CST tree.
	visitImports := func(n any) bool {
		inner, ok := n.(*syntax.InnerNode)
		if !ok {
			return true
		}
		switch inner.Kind {
		case syntax.SyntaxKindModuleImport:
			// parse import statement below.
		case syntax.SyntaxKindModuleInclude:
			// parse include statement below.
		default:
			return true
		}
		for _, child := range inner.Children {
			var leaf *syntax.LeafNode
			switch n := child.Repr.(type) {
			case *syntax.LeafNode:
				leaf = n
			case *syntax.InnerNode:
				if n.Kind != syntax.SyntaxKindParenthesized {
					continue
				}
				l, ok := findFirst(n.Children, syntax.SyntaxKindStr)
				if !ok {
					continue
				}
				leaf = l.Repr.(*syntax.LeafNode)
			}
			if leaf.Kind != syntax.SyntaxKindStr {
				continue
			}
			str, err := strconv.Unquote(leaf.Text)
			if err != nil {
				panic(err)
			}
			switch inner.Kind {
			case syntax.SyntaxKindModuleImport:
				spec, err := syntax.ParsePackageSpec(str)
				if err != nil {
					// NOTE: handle local import path e.g. `#import "foo/bar.typ"`
					path := str
					localTypPath := getRelLocalPath(relTypDir, path)
					localTypPaths = append(localTypPaths, localTypPath)
					return true // done with inner node
				}
				specs = append(specs, spec)
				if newSpec, ok := latestSpec(spec); ok {
					if len(libName) > 0 {
						clog.Infof("in library %q, update needed for %q:\n\tnew version %q\n\told version %q", libName, relTypPath, newSpec, spec)
					} else {
						clog.Infof("updated %q:\n\tnew version %q\n\told version %q", relTypPath, newSpec, spec)
						leaf.Text = strconv.Quote(newSpec.String())
					}
				}
				return true // done with inner node
			case syntax.SyntaxKindModuleInclude:
				path := str
				localTypPath := getRelLocalPath(relTypDir, path)
				localTypPaths = append(localTypPaths, localTypPath)
				return true // done with inner node
			}
		}
		return true
	}
	cstwalk.Walk(rootNode, visitImports)
	return specs, localTypPaths
}

var (
	// track libraries queued for processing.
	//
	// key: "@preview/latex-lookalike:0.1.4"
	todo = make(map[string]*syntax.PackageSpec)
	// track processed libraries.
	//
	// key: "@preview/latex-lookalike:0.1.4"
	done = make(map[string]*syntax.PackageSpec)
)

// checkLibs checks for and reports old transitive dependencies.
func checkLibs(specs []*syntax.PackageSpec) error {
	// fill queue.
	for _, spec := range specs {
		if _, ok := done[spec.String()]; ok {
			clog.Debugf("skipping library %q (already processed)", spec.String())
			continue // already processed.
		}
		todo[spec.String()] = spec
	}
	for {
		spec, ok := popSpec(todo)
		if !ok {
			break
		}
		clog.Debugf("processing library %q", spec.String())
		done[spec.String()] = spec
		libSpecs, err := checkLibWrapper(spec)
		if err != nil {
			return errors.WithStack(err)
		}
		for _, libSpec := range libSpecs {
			if _, ok := done[libSpec.String()]; ok {
				continue // already processed.
			}
			if _, ok := todo[libSpec.String()]; ok {
				continue // already in queue.
			}
			todo[libSpec.String()] = libSpec
		}
	}
	return nil
}

// checkLibWrapper checks the given package for old dependencies (transitively).
func checkLibWrapper(spec *syntax.PackageSpec) ([]*syntax.PackageSpec, error) {
	packageDir, err := getPackageDir(spec)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	projectRoot, err := os.OpenRoot(packageDir)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	manifest, err := getManifest(spec)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	relTypPath := manifest.Pkg.Entrypoint
	return checkLib(projectRoot, relTypPath, spec.String())
}

// checkLib checks the given package for old dependencies (transitively).
func checkLib(projectRoot *os.Root, relTypPath, libName string) ([]*syntax.PackageSpec, error) {
	// check if already processed.
	absTypPath := filepath.Join(projectRoot.Name(), relTypPath)
	if typDone[absTypPath] {
		clog.Debugf("skipping %q (already processed)", absTypPath)
		return nil, nil // already processed.
	}
	typDone[absTypPath] = true
	clog.Debugf("checking %q", relTypPath)

	// parse Typst source.
	rootNode, err := parse(projectRoot, relTypPath)
	if err != nil {
		return nil, errors.WithStack(err)
	}

	// Check for old versions of dependencies.
	specs, localTypPaths := findOldVersions(rootNode, relTypPath, libName)

	// Check local Typst files.
	for _, localTypPath := range localTypPaths {
		localSpecs, err := checkLib(projectRoot, localTypPath, libName)
		if err != nil {
			return nil, errors.WithStack(err)
		}
		specs = append(specs, localSpecs...)
	}

	return specs, nil
}

// parse parses the given Typst file.
func parse(root *os.Root, relTypPath string) (*syntax.SyntaxNode, error) {
	buf, err := root.ReadFile(relTypPath)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	text := string(buf)
	rootNode := syntax.Parse(text)
	return rootNode, nil
}

// ### [ Helper functions ] ####################################################

// getManifest returns the manifest of the given package.
func getManifest(spec *syntax.PackageSpec) (*syntax.PackageManifest, error) {
	pkgDir, err := getPackageDir(spec)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	pkgRoot, err := os.OpenRoot(pkgDir)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	manifest, err := parsePackageManifest(pkgRoot)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return manifest, nil
}

// parsePackageManifest parses the given package manifest file.
func parsePackageManifest(pkgRoot *os.Root) (*syntax.PackageManifest, error) {
	const manifestPath = "typst.toml"
	buf, err := pkgRoot.ReadFile(manifestPath)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	tomlContent := string(buf)
	manifest, err := syntax.ParsePackageManifestFromToml(tomlContent)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return manifest, nil
}

// latestSpec returns the latest version of the given package. The boolean
// return value indicates that a new version was available.
func latestSpec(spec *syntax.PackageSpec) (*syntax.PackageSpec, bool) {
	verSpecs := findVersions(spec)
	latestSpec := verSpecs[0]
	for _, verSpec := range verSpecs[1:] {
		if greaterThan(verSpec.Version, latestSpec.Version) {
			latestSpec = verSpec
		}
	}
	if latestSpec.Version == spec.Version {
		return spec, false
	}
	return latestSpec, true
}

// greaterThan reports whether version a is greater than version b.
func greaterThan(a, b syntax.PackageVersion) bool {
	if a.Major > b.Major {
		return true
	}
	if a.Major < b.Major {
		return false
	}
	if a.Minor > b.Minor {
		return true
	}
	if a.Minor < b.Minor {
		return false
	}
	return a.Patch > b.Patch
}

// findVersions finds all versions of the given package.
func findVersions(spec *syntax.PackageSpec) []*syntax.PackageSpec {
	var verSpecs []*syntax.PackageSpec
	packagesDirs := getPackagesDirs()
	for _, packagesDir := range packagesDirs {
		namespace := filepath.Base(packagesDir)
		packageDir := filepath.Join(packagesDir, spec.Name)
		visit := func(path string, info fs.FileInfo, err error) error {
			if err != nil {
				return err
			}
			if info.IsDir() || info.Mode()&fs.ModeSymlink != 0 {
				if path != packageDir {
					versionRaw := info.Name()
					version, err := syntax.ParsePackageVersion(versionRaw)
					if err != nil {
						return errors.WithStack(err)
					}
					verSpec := &syntax.PackageSpec{
						Namespace: namespace,
						Name:      spec.Name,
						Version:   version,
					}
					verSpecs = append(verSpecs, verSpec)
					// don't traverse into subdirectories.
					return filepath.SkipDir
				}
			}
			return nil
		}
		if err := filepath.Walk(packageDir, visit); err != nil {
			// TODO: report warning?
		}
	}
	return verSpecs
}

// getPackageDir returns the directory of the given package
// (e.g. "~/.local/share/typst/packages/local/hallon/0.1.2").
func getPackageDir(spec *syntax.PackageSpec) (string, error) {
	packagesDirs := getPackagesDirs()
	for _, packagesDir := range packagesDirs {
		packageDir := filepath.Join(packagesDir, spec.Name, spec.Version.String())
		if exists(packageDir) {
			return packageDir, nil
		}
	}
	return "", errors.Errorf("unable to locate package directory of %q", spec.String())
}

// getPackagesDirs returns the list of packages directories used by Typst for
// @local and @preview namespaces.
func getPackagesDirs() []string {
	var packagesDirs []string
	switch {
	// TODO: add support for Windows and Mac.
	case isUnix():
		// ~/.local/share
		homeDir, err := os.UserHomeDir()
		if err != nil {
			panic(err)
		}
		rootDir := filepath.Join(homeDir, ".local", "share")
		packagesDir := filepath.Join(rootDir, "typst", "packages", "local")
		packagesDirs = append(packagesDirs, packagesDir)

		// ~/.cache
		cacheDir, err := os.UserCacheDir()
		if err != nil {
			panic(err)
		}
		rootDir = cacheDir
		packagesDir = filepath.Join(rootDir, "typst", "packages", "preview")
		packagesDirs = append(packagesDirs, packagesDir)
	}
	// Typst universe 'packages' repo.
	if len(universeDir) > 0 {
		packagesDir := filepath.Join(universeDir, "packages", "preview")
		packagesDirs = append(packagesDirs, packagesDir)
	}
	return packagesDirs
}

// findFirst returns the first node of the given kind.
func findFirst(nodes []*syntax.SyntaxNode, kind syntax.SyntaxKind) (*syntax.SyntaxNode, bool) {
	for _, node := range nodes {
		if node.SyntaxKind() == kind {
			return node, true
		}
	}
	return nil, false
}

// getRelLocalPath returns the path relative to the given directory.
func getRelLocalPath(relTypDir, path string) string {
	if !filepath.IsAbs(path) {
		return filepath.Join(relTypDir, path)
	}
	rootSlash := fmt.Sprintf("%c", filepath.Separator)
	relLocalTypPath, err := filepath.Rel(rootSlash, path)
	if err != nil {
		panic(fmt.Errorf("unable to make Typst file path %q relative; %+v", path, err))
	}
	return relLocalTypPath
}

// stripPrefix strips the prefix from the given file path.
func stripPrefix(path, prefix string) (string, error) {
	path = filepath.Clean(path)
	prefix = filepath.Clean(prefix)
	sep := fmt.Sprintf("%c", filepath.Separator)
	prefixParts := strings.Split(prefix, sep)
	pathParts := strings.Split(path, sep)
	if len(pathParts) < len(prefixParts) {
		return "", errors.Errorf("project root (%q) not a prefix of Typst file %q", prefix, path)
	}
	for i := range prefixParts {
		if pathParts[i] != prefixParts[i] {
			return "", errors.Errorf("project root (%q) not a prefix of Typst file %q", prefix, path)
		}
	}
	//n := len(prefixParts)
	//relTypPath := filepath.Join(pathParts[n:]...)
	relTypPath, err := filepath.Rel(prefix, path)
	if err != nil {
		return "", errors.WithStack(err)
	}
	return relTypPath, nil
}

// popSpec returns a package specification from m if present. The boolean return
// value indicates success.
func popSpec(m map[string]*syntax.PackageSpec) (*syntax.PackageSpec, bool) {
	for _, spec := range m {
		delete(m, spec.String())
		return spec, true
	}
	return nil, false
}

// isUnix reports whether the current OS is an UNIX system.
//
//	"It's a Unix system, I know this!"
func isUnix() bool {
	goos := runtime.GOOS
	switch goos {
	case "js", "plan9", "wasip1", "windows":
		// Non-Unix.
		return false
	case "aix", "android", "darwin", "dragonfly", "freebsd", "illumos", "ios", "linux", "netbsd", "openbsd", "solaris":
		// Unix.
		return true
	}
	panic(fmt.Sprintf("support for %v not yet implemented", goos))
}

// exists reports whether the given file, directory or symlink exists.
func exists(path string) bool {
	_, err := os.Stat(path)
	return err == nil
}

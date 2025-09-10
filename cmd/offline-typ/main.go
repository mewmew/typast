// offline-typ rewrites Typst files for offline use.
package main

import (
	"bytes"
	"flag"
	"fmt"
	"io/fs"
	"log"
	"os"
	"path/filepath"
	"runtime"
	"slices"
	"strconv"
	"strings"

	"github.com/mewmew/typast/typst/cstwalk"
	"github.com/mewmew/typast/typst/syntax"
	"github.com/pkg/errors"
)

var (
	// outDir specifies the output directory.
	outDir string
	// projectRootDir specifies the project root directory.
	projectRootDir string
	// rawFontDirs specifies a colon-separated list of font directories.
	rawFontDirs string
	// verbose specifies whether to use verbose debug output.
	verbose bool
)

func usage() {
	flag.PrintDefaults()
	fmt.Fprintln(os.Stderr, "Usage: offline-typ [OPTION]... INPUT.typ")
}

func main() {
	// parse command line arguments.
	flag.Usage = usage
	flag.BoolVar(&verbose, "v", false, "verbose debug output")
	flag.StringVar(&outDir, "out", "out", "output directory")
	flag.StringVar(&projectRootDir, "root", "", "project root directory")
	flag.StringVar(&rawFontDirs, "font-path", "", "colon-separated list of font directories")
	flag.Parse()
	if flag.NArg() != 1 {
		flag.Usage()
		os.Exit(1)
	}
	typPath := flag.Arg(0)
	absTypPath, err := filepath.Abs(typPath)
	if err != nil {
		log.Fatalf("unable to get absolute path of %q; %+v", typPath, errors.WithStack(err))
	}
	var fontDirs []string
	if len(rawFontDirs) > 0 {
		fontDirs = strings.Split(rawFontDirs, ":")
	}

	// open project root directory.
	if len(projectRootDir) > 0 {
		absProjectRootDir, err := filepath.Abs(projectRootDir)
		if err != nil {
			log.Fatalf("unable to get absolute path of %q; %+v", projectRootDir, errors.WithStack(err))
		}
		projectRootDir = absProjectRootDir
		relTypPath, err := stripPrefix(absTypPath, absProjectRootDir)
		if err != nil {
			log.Fatalf("unable to strip project root prefix from Typst file path; %+v", err)
		}
		typPath = relTypPath
	} else {
		dir, file := filepath.Split(absTypPath)
		projectRootDir = dir
		typPath = file // relative path
	}
	projectRoot, err := os.OpenRoot(projectRootDir)
	if err != nil {
		log.Fatalf("unable to open project root directory; %+v", errors.WithStack(err))
	}

	// create output directory.
	if err := os.MkdirAll(outDir, 0o755); err != nil {
		log.Fatalf("%+v", err)
	}

	// open output root directory.
	outRoot, err := os.OpenRoot(outDir)
	if err != nil {
		log.Fatalf("unable to open output root directory; %+v", errors.WithStack(err))
	}

	// copy font paths.
	for _, fontDir := range fontDirs {
		if err := copyDir(outRoot, fontDir); err != nil {
			log.Fatalf("unable to copy font directory %q; %+v", fontDir, err)
		}
	}

	// rewrite Typst files for offline use.
	specs, err := rewrite(outRoot, projectRoot, typPath)
	if err != nil {
		log.Fatalf("%+v", err)
	}

	// copy packages to "/libs".
	if err := rewriteLibs(outRoot, specs); err != nil {
		log.Fatalf("%+v", err)
	}
	fmt.Printf("stored offline version in %q directory\n", outDir)
}

var (
	// key: "@preview/latex-lookalike:0.1.4"
	todo = make(map[string]*syntax.PackageSpec)
	// key: "@preview/latex-lookalike:0.1.4"
	done = make(map[string]*syntax.PackageSpec)
)

// rewriteLibs copies the given libraries to "/libs" and rewrites them for
// offline use.
func rewriteLibs(outRoot *os.Root, specs []*syntax.PackageSpec) error {
	// fill queue.
	for _, spec := range specs {
		if _, ok := done[spec.String()]; ok {
			if verbose {
				log.Fatalf("skipping library %q (already processed)")
			}
			continue // already processed.
		}
		todo[spec.String()] = spec
	}
	for {
		spec, ok := popSpec(todo)
		if !ok {
			break
		}
		if verbose {
			log.Printf("rewriting library %q", spec.String())
		}
		done[spec.String()] = spec
		libSpecs, err := copyPackage(outRoot, spec)
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

// track Typst files (full path) that have been rewritten.
var typDone = make(map[string]bool)

// rewrite rewrites the given Typst file for offline use.
func rewrite(outRoot, projectRoot *os.Root, relTypPath string) ([]*syntax.PackageSpec, error) {
	absTypPath := filepath.Join(projectRoot.Name(), relTypPath)
	if typDone[absTypPath] {
		log.Printf("skipping %q (already processed)", absTypPath)
		return nil, nil // already processed.
	}
	typDone[absTypPath] = true
	if verbose {
		log.Printf("rewriting %q", relTypPath)
	}
	relTypDir := filepath.Dir(relTypPath)
	if err := outRoot.MkdirAll(relTypDir, 0o755); err != nil {
		return nil, errors.WithStack(err)
	}
	// parse Typst source.
	rootNode, err := parse(projectRoot, relTypPath)
	if err != nil {
		return nil, errors.WithStack(err)
	}

	// print tree.
	//syntax.PrintRoot(rootNode)

	var (
		// package imports to convert for offline use.
		specs []*syntax.PackageSpec
		// local Typst files to convert for offline use.
		localTypPaths []string
		// local resource paths to copy for offline use (e.g. images).
		localResourcePaths []string
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
		for i, child := range inner.Children {
			leaf, ok := child.Repr.(*syntax.LeafNode)
			if !ok {
				continue
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
				newPkgPath, err := getNewPkgPath(spec)
				if err != nil {
					panic(fmt.Errorf("unable to get new package path for package %q; %+v", spec.String(), errors.WithStack(err)))
				}
				leaf.Text = strconv.Quote(newPkgPath)
				insertPkgRename(inner, i+1, spec.Name)
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

	visitResourceFunc := func(n any) bool {
		inner, ok := n.(*syntax.InnerNode)
		if !ok {
			return true
		}
		switch inner.Kind {
		case syntax.SyntaxKindFuncCall:
			// parse function call below.
		default:
			return true
		}
		var foundFunc string
		for _, child := range inner.Children {
			if leaf, ok := child.Repr.(*syntax.LeafNode); ok {
				if leaf.Kind == syntax.SyntaxKindIdent {
					foundFunc = leaf.Text
					continue
				}
			}
			switch foundFunc {
			case "image":
				// call to image function handled below.
			case "bibliography":
				// call to bibliography function handled below.
			default:
				continue // skip other function calls
			}
			if args, ok := child.Repr.(*syntax.InnerNode); ok {
				if args.Kind != syntax.SyntaxKindArgs {
					continue
				}
				if path, ok := findFirstStr(args.Children); ok {
					localResourcePath := getRelLocalPath(relTypDir, path)
					localResourcePaths = append(localResourcePaths, localResourcePath)
				}
				if foundFunc == "bibliography" {
					if kv, ok := findFirstNamed(args.Children); ok {
						if kv.Key == "style" {
							cslPath := kv.Value
							localResourcePath := getRelLocalPath(relTypDir, cslPath)
							if exist(projectRoot, localResourcePath) {
								localResourcePaths = append(localResourcePaths, localResourcePath)
							}
						}
					}
				}
			}
		}
		return true
	}
	cstwalk.Walk(rootNode, visitResourceFunc)

	visitResourceSetRule := func(n any) bool {
		inner, ok := n.(*syntax.InnerNode)
		if !ok {
			return true
		}
		switch inner.Kind {
		case syntax.SyntaxKindSetRule:
			// parse set rule below.
		default:
			return true
		}
		var foundFunc string
		for _, child := range inner.Children {
			if leaf, ok := child.Repr.(*syntax.LeafNode); ok {
				if leaf.Kind == syntax.SyntaxKindIdent {
					foundFunc = leaf.Text
					continue
				}
			}
			switch foundFunc {
			case "bibliography":
				// handle bibliography set rule below.
			default:
				continue // skip other function calls
			}
			if args, ok := child.Repr.(*syntax.InnerNode); ok {
				if args.Kind != syntax.SyntaxKindArgs {
					continue
				}
				if path, ok := findFirstStr(args.Children); ok {
					localResourcePath := getRelLocalPath(relTypDir, path)
					localResourcePaths = append(localResourcePaths, localResourcePath)
				}
				if foundFunc == "bibliography" {
					if kv, ok := findFirstNamed(args.Children); ok {
						if kv.Key == "style" {
							cslPath := kv.Value
							localResourcePath := getRelLocalPath(relTypDir, cslPath)
							if exist(projectRoot, localResourcePath) {
								localResourcePaths = append(localResourcePaths, localResourcePath)
							}
						}
					}
				}
			}
		}
		return true
	}
	cstwalk.Walk(rootNode, visitResourceSetRule)

	// Convert local Typst files.
	for _, localTypPath := range localTypPaths {
		localSpecs, err := rewrite(outRoot, projectRoot, localTypPath)
		if err != nil {
			return nil, errors.WithStack(err)
		}
		specs = append(specs, localSpecs...)
	}

	// Copy local resources.
	for _, localResourcePath := range localResourcePaths {
		buf, err := projectRoot.ReadFile(localResourcePath)
		if err != nil {
			return nil, errors.WithStack(err)
		}
		dir := filepath.Dir(localResourcePath)
		if err := outRoot.MkdirAll(dir, 0o755); err != nil {
			return nil, errors.WithStack(err)
		}
		if err := outRoot.WriteFile(localResourcePath, buf, 0o644); err != nil {
			return nil, errors.WithStack(err)
		}
	}

	// output new version of file.
	out := &bytes.Buffer{}
	syntax.PrintNode(out, rootNode)
	if err := outRoot.WriteFile(relTypPath, out.Bytes(), 0o644); err != nil {
		return nil, errors.WithStack(err)
	}
	return specs, nil
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

type KeyValuePair struct {
	Key   string
	Value string
}

// findFirstNamed returns the key-value pair of the first named pair node.
func findFirstNamed(nodes []*syntax.SyntaxNode) (KeyValuePair, bool) {
	node, ok := findFirst(nodes, syntax.SyntaxKindNamed)
	if !ok {
		return KeyValuePair{}, false
	}
	inner, ok := node.Repr.(*syntax.InnerNode)
	if !ok {
		return KeyValuePair{}, false
	}
	key, ok := findFirstIdent(inner.Children)
	if !ok {
		return KeyValuePair{}, false
	}
	value, ok := findFirstStr(inner.Children)
	if !ok {
		return KeyValuePair{}, false
	}
	kv := KeyValuePair{
		Key:   key,
		Value: value,
	}
	return kv, true
}

// findFirstIdent returns the name of the first identifier node.
func findFirstIdent(nodes []*syntax.SyntaxNode) (string, bool) {
	node, ok := findFirst(nodes, syntax.SyntaxKindIdent)
	if !ok {
		return "", false
	}
	leaf, ok := node.Repr.(*syntax.LeafNode)
	if !ok {
		return "", false
	}
	return leaf.Text, true
}

// findFirstStr returns the unquoted version of the first string literal node.
func findFirstStr(nodes []*syntax.SyntaxNode) (string, bool) {
	node, ok := findFirst(nodes, syntax.SyntaxKindStr)
	if !ok {
		return "", false
	}
	leaf, ok := node.Repr.(*syntax.LeafNode)
	if !ok {
		return "", false
	}
	text, err := strconv.Unquote(leaf.Text)
	if err != nil {
		// TODO: report error?
		return "", false
	}
	return text, true
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

// insertPkgRename inserts a package rename directive (if not already present)
// at the specified index in the given import statement.
func insertPkgRename(inner *syntax.InnerNode, index int, pkgName string) {
	for _, child := range inner.Children[index:] {
		switch child.SyntaxKind() {
		case syntax.SyntaxKindAs:
			return // pkg already renamed.
		case syntax.SyntaxKindColon:
			break // insert package rename at index (before identifier import).
		}
	}
	space1 := syntax.NewLeaf(syntax.SyntaxKindSpace, " ")
	as := syntax.NewLeaf(syntax.SyntaxKindAs, "as")
	space2 := syntax.NewLeaf(syntax.SyntaxKindSpace, " ")
	pkgNameNode := syntax.NewLeaf(syntax.SyntaxKindStr, pkgName)
	// ` as "latex-lookalike"`
	inner.Children = slices.Insert(inner.Children, index, space1, as, space2, pkgNameNode)
}

// copyPackage copies the given package to a corresponding project-rooted
// offline path.
func copyPackage(outRoot *os.Root, spec *syntax.PackageSpec) ([]*syntax.PackageSpec, error) {
	pkgDir, err := getPackageDir(spec)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	pkgRoot, err := os.OpenRoot(pkgDir)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	libTypDir := fmt.Sprintf("libs/%v/%v", spec.Name, spec.Version.String())
	if err := outRoot.MkdirAll(libTypDir, 0o755); err != nil {
		return nil, errors.WithStack(err)
	}
	outLibRoot, err := outRoot.OpenRoot(libTypDir)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	manifest, err := getManifest(spec)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	pkgInfo := manifest.Pkg
	libSpecs, err := rewrite(outLibRoot, pkgRoot, pkgInfo.Entrypoint)
	if err != nil {
		return nil, errors.WithStack(err)
	}

	return libSpecs, nil
}

// getNewPkgPath returns the project-rooted offline path of the given package.
func getNewPkgPath(spec *syntax.PackageSpec) (string, error) {
	manifest, err := getManifest(spec)
	if err != nil {
		return "", errors.WithStack(err)
	}
	newPkgPath := fmt.Sprintf("/libs/%v/%v/%s", spec.Name, spec.Version.String(), manifest.Pkg.Entrypoint)
	return newPkgPath, nil
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

// getPackageDir returns the directory containing the given package.
func getPackageDir(spec *syntax.PackageSpec) (string, error) {
	var rootDir string
	switch {
	case isUnix() && spec.Namespace == "local":
		// ~/.local/share
		homeDir, err := os.UserHomeDir()
		if err != nil {
			return "", errors.WithStack(err)
		}
		rootDir = filepath.Join(homeDir, ".local", "share")
	default:
		// ~/.cache
		cacheDir, err := os.UserCacheDir()
		if err != nil {
			return "", errors.WithStack(err)
		}
		rootDir = cacheDir
	}
	pkgDir := filepath.Join(rootDir, "typst", "packages", spec.Namespace, spec.Name, spec.Version.String())
	return pkgDir, nil
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

// exist reports whether the given file exists within root.
func exist(root *os.Root, path string) bool {
	_, err := root.Stat(path)
	return err == nil
}

// copyDir copies the given directory to the root file system.
func copyDir(root *os.Root, dir string) error {
	visit := func(path string, info fs.FileInfo, err error) error {
		if info.IsDir() {
			if err := root.MkdirAll(path, 0o755); err != nil {
				return errors.WithStack(err)
			}
		}
		if info.Mode().IsRegular() {
			buf, err := os.ReadFile(path)
			if err != nil {
				return errors.WithStack(err)
			}
			if err := root.WriteFile(path, buf, 0o644); err != nil {
				return errors.WithStack(err)
			}
		}
		return nil
	}
	if err := filepath.Walk(dir, visit); err != nil {
		return errors.WithStack(err)
	}
	return nil
}

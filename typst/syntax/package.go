// Package manifest parsing.

package syntax

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mewmew/typast/internal/scanner"
	internaltoml "github.com/mewmew/typast/internal/toml"
	"github.com/pelletier/go-toml/v2"
	"github.com/pkg/errors"
)

// A type alias for a map of key-value pairs used to collect unknown fields.
type UnknownFields map[string]any

// A parsed package manifest.
//
// The `UnknownFields` contains fields which were found but not expected.
type PackageManifest struct {
	// Details about the package itself.
	Pkg *PackageInfo `toml:"package"`
	// Details about the template, if the package is one.
	Template *TemplateInfo `toml:"template,omitempty"`
	// The tools section for third-party configuration.
	Tool *ToolInfo `toml:"tool,omitempty"`
	// All parsed but unknown fields, this can be used for validation.
	UnknownFields UnknownFields `toml:"-"`
}

func ParsePackageManifestFromToml(content string) (*PackageManifest, error) {
	manifest := &PackageManifest{}
	if err := toml.Unmarshal([]byte(content), manifest); err != nil {
		return nil, errors.WithStack(err)
	}
	return manifest, nil
}

// The `[tool]` key in the manifest. This field can be used to retrieve
// 3rd-party tool configuration.
type ToolInfo struct {
	// Any fields parsed in the tool section.
	Sections map[string]internaltoml.Table
}

// The `[template]` key in the manifest.
//
// The `UnknownFields` contains fields which were found but not expected.
type TemplateInfo struct {
	// The directory within the package that contains the files that should be
	// copied into the user's new project directory.
	Path string `toml:"path"`
	// A path relative to the template's path that points to the file serving as
	// the compilation target.
	Entrypoint string `toml:"entrypoint"`
	// A path relative to the package's root that points to a PNG or lossless
	// WebP thumbnail for the template.
	Thumbnail string `toml:"thumbnail,omitempty"`
	// All parsed but unknown fields, this can be used for validation.
	UnknownFields UnknownFields `toml:"-"`
}

// The `[package]` key in the manifest.
//
// The `UnknownFields` contains fields which were found but not expected.
type PackageInfo struct {
	// The name of the package within its namespace.
	Name string `toml:"name"`
	// The package's version.
	Version PackageVersion `toml:"version"`
	// The path of the entrypoint into the package.
	Entrypoint string `toml:"entrypoint"`
	// A list of the package's authors.
	Authors []string `toml:"authors,omitempty"`
	// The package's license.
	License string `toml:"license,omitempty"`
	// A short description of the package.
	Description string `toml:"description,omitempty"`
	// A link to the package's web presence.
	Homepage string `toml:"homepage,omitempty"`
	// A link to the repository where this package is developed.
	Repository string `toml:"repository,omitempty"`
	// An array of search keywords for the package.
	Keywords []string `toml:"keywords,omitempty"`
	// An array with up to three of the predefined categories to help users
	// discover the package.
	Categories []string `toml:"categories,omitempty"`
	// An array of disciplines defining the target audience for which the package
	// is useful.
	Disciplines []string `toml:"disciplines,omitempty"`
	// The minimum required compiler version for the package.
	Compiler *VersionBound `toml:"compiler,omitempty"`
	// An array of globs specifying files that should not be part of the
	// published bundle.
	Exclude []string `toml:"exclude,omitempty"`
	// All parsed but unknown fields, this can be used for validation.
	UnknownFields UnknownFields `toml:"-"`
}

// NewPackageManifest creates a new package manifest with the given package
// info.
func NewPackageManifest(pkg *PackageInfo) *PackageManifest {
	return &PackageManifest{
		Pkg:      pkg,
		Template: nil,
		Tool: &ToolInfo{
			Sections: make(map[string]internaltoml.Table),
		},
		UnknownFields: make(UnknownFields),
	}
}

// Validate ensures that this manifest is indeed for the specified package.
func (manifest *PackageManifest) Validate(spec *PackageSpec) error {
	if manifest.Pkg.Name != spec.Name {
		return errors.Errorf("package manifest contains mismatched name %q", manifest.Pkg.Name)
	}

	if manifest.Pkg.Version != spec.Version {
		return errors.Errorf("package manifest contains mismatched version %v", manifest.Pkg.Version)
	}

	if manifest.Pkg.Compiler != nil {
		current := CurrentCompilerVersion()
		if !current.MatchesGE(manifest.Pkg.Compiler) {
			return errors.Errorf(
				"package requires typst %v or newer (current version is %v)",
				manifest.Pkg.Compiler,
				current,
			)
		}
	}

	return nil
}

// NewTemplateInfo creates a new template info with only required fields.
func NewTemplateInfo(path, entrypoint string) *TemplateInfo {
	return &TemplateInfo{
		Path:          path,
		Entrypoint:    entrypoint,
		Thumbnail:     "",
		UnknownFields: make(UnknownFields),
	}
}

// NewPackageInfo creates a new package info with only required fields.
func NewPackageInfo(name string, version PackageVersion, entrypoint string) *PackageInfo {
	return &PackageInfo{
		Name:          name,
		Version:       version,
		Entrypoint:    entrypoint,
		UnknownFields: make(UnknownFields),
	}
}

// PackageSpec identifies a package.
type PackageSpec struct {
	// The namespace the package lives in.
	Namespace string
	// The name of the package within its namespace.
	Name string
	// The package's version.
	Version PackageVersion
}

func (spec *PackageSpec) Versionless() *VersionlessPackageSpec {
	return &VersionlessPackageSpec{
		Namespace: spec.Namespace,
		Name:      spec.Name,
	}
}

// ParsePackageSpec parses a package spec from string.
func ParsePackageSpec(str string) (*PackageSpec, error) {
	s := scanner.New(str)
	namespace, err := parseNamespace(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	name, err := parseName(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	version, err := parseVersion(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return &PackageSpec{Namespace: namespace, Name: name, Version: version}, nil
}

func (spec *PackageSpec) String() string {
	return fmt.Sprintf("@%s/%s:%s", spec.Namespace, spec.Name, spec.Version)
}

func (spec *PackageSpec) Clone() *PackageSpec {
	return &PackageSpec{
		Namespace: spec.Namespace,
		Name:      spec.Name,
		Version:   spec.Version,
	}
}

// VersionlessPackageSpec identifies a package, but not a specific version.
type VersionlessPackageSpec struct {
	// The namespace the package lives in.
	Namespace string
	// The name of the package within its namespace.
	Name string
}

// At fills in the version to get a complete PackageSpec.
func (spec *VersionlessPackageSpec) At(version PackageVersion) *PackageSpec {
	return &PackageSpec{
		Namespace: spec.Namespace,
		Name:      spec.Name,
		Version:   version,
	}
}

// ParseVersionlessPackageSpec parses a versionless package spec from string.
func ParseVersionlessPackageSpec(str string) (*VersionlessPackageSpec, error) {
	s := scanner.New(str)
	namespace, err := parseNamespace(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	name, err := parseName(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	if !s.Done() {
		return nil, errors.New("unexpected version in versionless package specification")
	}
	return &VersionlessPackageSpec{Namespace: namespace, Name: name}, nil
}

func (spec *VersionlessPackageSpec) String() string {
	return fmt.Sprintf("@%s/%s", spec.Namespace, spec.Name)
}

func parseNamespace(s *scanner.Scanner) (string, error) {
	if !s.EatIf("@") {
		return "", errors.New("package specification must start with '@'")
	}
	namespace := s.EatUntil("/")
	if len(namespace) == 0 {
		return "", errors.New("package specification is missing namespace")
	} else if !isIdent(namespace) {
		return "", errors.Errorf("%q is not a valid package namespace", namespace)
	}
	return namespace, nil
}

func parseName(s *scanner.Scanner) (string, error) {
	s.EatIf("/")
	name := s.EatUntil(":")
	if len(name) == 0 {
		return "", errors.New("package specification is missing name")
	} else if !isIdent(name) {
		return "", errors.Errorf("%q is not a valid package name", name)
	}
	return name, nil
}

func parseVersion(s *scanner.Scanner) (PackageVersion, error) {
	s.EatIf(":")
	version := s.After()
	if len(version) == 0 {
		return PackageVersion{}, errors.New("package specification is missing version")
	}
	return ParsePackageVersion(version)
}

// PackageVersion represents a package's version.
type PackageVersion struct {
	Major uint32
	Minor uint32
	Patch uint32
}

func (v *PackageVersion) UnmarshalText(text []byte) error {
	version, err := ParsePackageVersion(string(text))
	if err != nil {
		return errors.WithStack(err)
	}
	*v = version
	return nil
}

// ParsePackageVersion parses a version string.
func ParsePackageVersion(str string) (PackageVersion, error) {
	parts := strings.Split(str, ".")
	if len(parts) < 3 {
		return PackageVersion{}, errors.New("version number must have major.minor.patch")
	}
	major, err := strconv.ParseUint(parts[0], 10, 32)
	if err != nil {
		return PackageVersion{}, errors.Wrapf(err, "%q is not a valid major version", parts[0])
	}
	minor, err := strconv.ParseUint(parts[1], 10, 32)
	if err != nil {
		return PackageVersion{}, errors.Wrapf(err, "%q is not a valid minor version", parts[1])
	}
	patch, err := strconv.ParseUint(parts[2], 10, 32)
	if err != nil {
		return PackageVersion{}, errors.Wrapf(err, "%q is not a valid patch version", parts[2])
	}
	if len(parts) > 3 {
		return PackageVersion{}, errors.Errorf("version number has unexpected components: %q", parts[3:])
	}
	return PackageVersion{Major: uint32(major), Minor: uint32(minor), Patch: uint32(patch)}, nil
}

func (v PackageVersion) String() string {
	return fmt.Sprintf("%d.%d.%d", v.Major, v.Minor, v.Patch)
}

// MatchesEQ performs an `==` match with the given version bound. Version
// elements missing in the bound are ignored.
func (v PackageVersion) MatchesEQ(bound *VersionBound) bool {
	if v.Major != bound.Major {
		return false
	}
	if bound.Minor != nil && v.Minor != *bound.Minor {
		return false
	}
	if bound.Patch != nil && v.Patch != *bound.Patch {
		return false
	}
	return true
}

// MatchesGT performs a `>` match with the given version bound. The match only
// succeeds if some version element in the bound is actually greater than that
// of the version.
func (v PackageVersion) MatchesGT(bound *VersionBound) bool {
	if v.Major != bound.Major {
		return v.Major > bound.Major
	}
	if bound.Minor == nil {
		return false
	}
	if v.Minor != *bound.Minor {
		return v.Minor > *bound.Minor
	}
	if bound.Patch == nil {
		return false
	}
	return v.Patch > *bound.Patch
}

// MatchesLT performs a `<` match with the given version bound. The match only
// succeeds if some version element in the bound is actually less than that of
// the version.
func (v PackageVersion) MatchesLT(bound *VersionBound) bool {
	if v.Major != bound.Major {
		return v.Major < bound.Major
	}
	if bound.Minor == nil {
		return false
	}
	if v.Minor != *bound.Minor {
		return v.Minor < *bound.Minor
	}
	if bound.Patch == nil {
		return false
	}
	return v.Patch < *bound.Patch
}

// MatchesGE performs a `>=` match with the given versions. The match succeeds
// when either a `==` or `>` match does.
func (v PackageVersion) MatchesGE(bound *VersionBound) bool {
	return v.MatchesEQ(bound) || v.MatchesGT(bound)
}

// MatchesLE performs a `<=` match with the given versions. The match succeeds
// when either a `==` or `<` match does.
func (v PackageVersion) MatchesLE(bound *VersionBound) bool {
	return v.MatchesEQ(bound) || v.MatchesLT(bound)
}

func CurrentCompilerVersion() PackageVersion {
	return PackageVersion{Major: 0, Minor: 13, Patch: 1}
}

// VersionBound represents a version bound for compatibility specification.
type VersionBound struct {
	// The bounds's major version.
	Major uint32
	// The bounds's minor version.
	Minor *uint32
	// The bounds's patch version. Can only be present if minor is too.
	Patch *uint32
}

// ParseVersionBound parses a version bound from string.
func ParseVersionBound(str string) (*VersionBound, error) {
	parts := strings.Split(str, ".")
	if len(parts) == 0 {
		return nil, errors.New("version string is empty")
	}

	major, err := strconv.ParseUint(parts[0], 10, 32)
	if err != nil {
		return nil, errors.Wrapf(err, "%q is not a valid major version", parts[0])
	}

	var minor, patch *uint32
	if len(parts) > 1 {
		m, err := strconv.ParseUint(parts[1], 10, 32)
		if err != nil {
			return nil, errors.Wrapf(err, "%q is not a valid minor version", parts[1])
		}
		x := uint32(m)
		minor = &x
	}
	if len(parts) > 2 {
		p, err := strconv.ParseUint(parts[2], 10, 32)
		if err != nil {
			return nil, errors.Wrapf(err, "%q is not a valid patch version", parts[2])
		}
		y := uint32(p)
		patch = &y
	}
	if len(parts) > 3 {
		return nil, errors.Errorf("version number has unexpected components: %q", parts[3:])
	}

	return &VersionBound{Major: uint32(major), Minor: minor, Patch: patch}, nil
}

func (v *VersionBound) String() string {
	out := &strings.Builder{}
	fmt.Fprintf(out, "%d", v.Major)
	if v.Minor != nil {
		fmt.Fprintf(out, ".%d", *v.Minor)
	}
	if v.Patch != nil {
		fmt.Fprintf(out, ".%d", *v.Patch)
	}
	return out.String()
}

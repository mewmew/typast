// Package manifest parsing.

package syntax

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/scanner"
	internaltoml "github.com/mewmew/typast/internal/toml"
	toml "github.com/pelletier/go-toml/v2"
	"github.com/pkg/errors"
)

// TODO: figure out type.
type IgnoredAny any

// A type alias for a map of key-value pairs used to collect unknown fields
// where values are completely discarded.
type UnknownFields map[string]IgnoredAny

// A parsed package manifest.
//
// The `unknown_fields` contains fields which were found but not expected.
type PackageManifest struct {
	// Details about the package itself.
	Pkg *PackageInfo `toml:"package"`
	// Details about the template, if the package is one.
	template option.Option[*TemplateInfo]
	// The tools section for third-party configuration.
	tool *ToolInfo
	// All parsed but unknown fields, this can be used for validation.
	unknown_fields UnknownFields
}

func ParsePackageManifestFromToml(content string) (*PackageManifest, error) {
	manifest := &PackageManifest{}
	err := toml.Unmarshal([]byte(content), manifest)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return manifest, nil
}

// The `[tool]` key in the manifest. This field can be used to retrieve
// 3rd-party tool configuration.
//
// # Examples
// ```
// # use serde::{Deserialize, Serialize};
// # use ecow::string;
// # use typst_syntax::package::PackageManifest;
// #[derive(Debug, PartialEq, Serialize, Deserialize)]
//
//	struct MyTool {
//	    key: string,
//	}
//
// let mut manifest: PackageManifest = toml::from_str(r#"
//
//	[package]
//	name = "package"
//	version = "0.1.0"
//	entrypoint = "src/lib.typ"
//
//	[tool.my-tool]
//	key = "value"
//
// "#)?;
//
// let my_tool = manifest
//
//	.tool
//	.sections
//	.remove("my-tool")
//	.ok_or("tool.my-tool section missing")?;
//
// let my_tool = MyTool::deserialize(my_tool)?;
//
// assert_eq!(my_tool, MyTool { key: "value".into() });
// # Ok::<_, Box<dyn std::error::Error>>(())
// ```
type ToolInfo struct {
	// Any fields parsed in the tool section.
	sections map[string]internaltoml.Table
}

// The `[template]` key in the manifest.
//
// The `unknown_fields` contains fields which were found but not expected.
type TemplateInfo struct {
	// The directory within the package that contains the files that should be
	// copied into the user's new project directory.
	path string
	// A path relative to the template's path that points to the file serving
	// as the compilation target.
	entrypoint string
	// A path relative to the package's root that points to a PNG or lossless
	// WebP thumbnail for the template.
	thumbnail option.Option[string]
	// All parsed but unknown fields, this can be used for validation.
	unknown_fields UnknownFields
}

// The `[package]` key in the manifest.
//
// The `unknown_fields` contains fields which were found but not expected.
type PackageInfo struct {
	// The name of the package within its namespace.
	Name string `toml:"name"`
	// The package's version.
	Version PackageVersion `toml:"version"`
	// The path of the entrypoint into the package.
	Entrypoint string `toml:"entrypoint"`
	// A list of the package's authors.
	authors []string
	//  The package's license.
	license option.Option[string]
	// A short description of the package.
	description option.Option[string]
	// A link to the package's web presence.
	homepage option.Option[string]
	// A link to the repository where this package is developed.
	repository option.Option[string]
	// An array of search keywords for the package.
	keywords []string
	// An array with up to three of the predefined categories to help users
	// discover the package.
	categories []string
	// An array of disciplines defining the target audience for which the
	// package is useful.
	disciplines []string
	// The minimum required compiler version for the package.
	compiler option.Option[*VersionBound]
	// An array of globs specifying files that should not be part of the
	// published bundle.
	exclude []string
	// All parsed but unknown fields, this can be used for validation.
	unknown_fields UnknownFields
}

// --- [ PackageManifest ] -----------------------------------------------------

// Create a new package manifest with the given package info.
//
// new
func NewPackageManifest(pkg *PackageInfo) *PackageManifest {
	return &PackageManifest{
		Pkg:      pkg,
		template: option.None[*TemplateInfo](),
		tool: &ToolInfo{
			sections: make(map[string]internaltoml.Table),
		},
		unknown_fields: make(UnknownFields),
	}
}

// Ensure that this manifest is indeed for the specified package.
func (manifest *PackageManifest) validate(spec *PackageSpec) error {
	if manifest.Pkg.Name != spec.Name {
		return errors.Errorf(
			"package manifest contains mismatched name `%v`",
			manifest.Pkg.Name,
		)
	}

	if manifest.Pkg.Version != spec.Version {
		return errors.Errorf(
			"package manifest contains mismatched version %v",
			manifest.Pkg.Version,
		)
	}

	if required, ok := manifest.Pkg.compiler.Get(); ok {
		current := CurrentCompilerVersion()
		if !current.matches_ge(required) {
			return errors.Errorf(
				"package requires typst %v or newer (current version is %v)",
				required,
				current,
			)
		}
	}

	return nil
}

// --- [/ PackageManifest ] ----------------------------------------------------

// --- [ TemplateInfo ] --------------------------------------------------------

// Create a new template info with only required fields.
//
// new
func NewTemplateInfo(path string, entrypoint string) *TemplateInfo {
	return &TemplateInfo{
		path:           path,
		entrypoint:     entrypoint,
		thumbnail:      option.None[string](),
		unknown_fields: make(UnknownFields),
	}
}

// --- [/ TemplateInfo ] -------------------------------------------------------

// --- [ PackageInfo ] ---------------------------------------------------------

// Create a new package info with only required fields.
//
// new
func NewPackageInfo(
	name string,
	version PackageVersion,
	entrypoint string,
) *PackageInfo {
	return &PackageInfo{
		Name:           name,
		Version:        version,
		Entrypoint:     entrypoint,
		authors:        nil,
		categories:     nil,
		compiler:       option.None[*VersionBound](),
		description:    option.None[string](),
		disciplines:    nil,
		exclude:        nil,
		homepage:       option.None[string](),
		keywords:       nil,
		license:        option.None[string](),
		repository:     option.None[string](),
		unknown_fields: make(UnknownFields),
	}
}

// --- [ /PackageInfo ] --------------------------------------------------------

// Identifies a package.
type PackageSpec struct {
	// The namespace the package lives in.
	Namespace string
	// The name of the package within its namespace.
	Name string
	// The package's version.
	Version PackageVersion
}

func (spec *PackageSpec) versionless() *VersionlessPackageSpec {
	return &VersionlessPackageSpec{
		namespace: spec.Namespace,
		name:      spec.Name,
	}
}

// FromStr for PackageSpec
func ParsePackageSpec(str string) (*PackageSpec, error) {
	s := scanner.New(str)
	namespace, err := parse_namespace(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	name, err := parse_name(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	version, err := parse_version(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	return &PackageSpec{
		Namespace: namespace,
		Name:      name,
		Version:   version,
	}, nil
}

func (spec *PackageSpec) String() string {
	return fmt.Sprintf("@%s/%s:%s", spec.Namespace, spec.Name, spec.Version)
}

func (spec *PackageSpec) clone() *PackageSpec {
	return &PackageSpec{
		Namespace: spec.Namespace,
		Name:      spec.Name,
		Version:   spec.Version,
	}
}

// Identifies a package, but not a specific version of it.
type VersionlessPackageSpec struct {
	// The namespace the package lives in.
	namespace string
	// The name of the package within its namespace.
	name string
}

// Fill in the `version` to get a complete [`PackageSpec`].
func (spec *VersionlessPackageSpec) at(version PackageVersion) *PackageSpec {
	return &PackageSpec{
		Namespace: spec.namespace,
		Name:      spec.name,
		Version:   version,
	}
}

// FromStr for VersionlessPackageSpec
func ParseVersionlessPackageSpec(str string) (*VersionlessPackageSpec, error) {
	s := scanner.New(str)
	namespace, err := parse_namespace(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	name, err := parse_name(s)
	if err != nil {
		return nil, errors.WithStack(err)
	}
	if !s.Done() {
		return nil, errors.New("unexpected version in versionless package specification")
	}
	return &VersionlessPackageSpec{
		namespace: namespace,
		name:      name,
	}, nil
}

func (spec *VersionlessPackageSpec) String() string {
	return fmt.Sprintf("@%s/%s", spec.namespace, spec.name)
}

func parse_namespace(s *scanner.Scanner) (string, error) {
	if !s.EatIf("@") {
		return "", errors.New("package specification must start with '@'")
	}

	namespace := s.EatUntil("/")
	if len(namespace) == 0 {
		return "", errors.New("package specification is missing namespace")
	} else if !is_ident(namespace) {
		return "", errors.Errorf("`%v` is not a valid package namespace", namespace)
	}

	return namespace, nil
}

func parse_name(s *scanner.Scanner) (string, error) {
	s.EatIf("/")

	name := s.EatUntil(":")
	if len(name) == 0 {
		return "", errors.New("package specification is missing name")
	} else if !is_ident(name) {
		return "", errors.Errorf("`%v` is not a valid package name", name)
	}

	return name, nil
}

func parse_version(s *scanner.Scanner) (PackageVersion, error) {
	s.EatIf(":")

	version := s.After()
	if len(version) == 0 {
		return PackageVersion{}, errors.New("package specification is missing version")
	}

	return ParsePackageVersion(version)
}

// A package's version.
type PackageVersion struct {
	// The package's major version.
	major uint32
	// The package's minor version.
	minor uint32
	// The package's patch version.
	patch uint32
}

func (v *PackageVersion) UnmarshalText(text []byte) error {
	version, err := ParsePackageVersion(string(text))
	if err != nil {
		return errors.WithStack(err)
	}
	*v = version
	return nil
}

// FromStr for PackageVersion
func ParsePackageVersion(str string) (PackageVersion, error) {
	parts := strings.Split(str, ".")
	kinds := []string{"major", "minor", "patch"}
	var major, minor, patch uint32
	for i, kind := range kinds {
		if len(parts) <= i {
			return PackageVersion{}, errors.Errorf("version number is missing %v version", kind)
		}
		part := parts[i]
		x, err := strconv.ParseUint(part, 10, 32)
		if err != nil {
			return PackageVersion{}, errors.Wrapf(err, "`%v` is not a valid %v version", part, kind)
		}
		switch kind {
		case "major":
			major = uint32(x)
		case "minor":
			minor = uint32(x)
		case "patch":
			patch = uint32(x)
		}
	}
	if len(parts) > 3 {
		return PackageVersion{}, errors.Errorf("version number has unexpected components: `%v`", parts[3:])
	}
	return PackageVersion{
		major: major,
		minor: minor,
		patch: patch,
	}, nil
}

func (version PackageVersion) String() string {
	return fmt.Sprintf("%d.%d.%d", version.major, version.minor, version.patch)
}

// Performs an `==` match with the given version bound. Version elements
// missing in the bound are ignored.
func (version PackageVersion) matches_eq(bound *VersionBound) bool {
	if version.major != bound.major {
		return false
	}
	if minor, ok := bound.minor.Get(); ok {
		if version.minor != minor {
			return false
		}
	}
	if patch, ok := bound.patch.Get(); ok {
		if version.patch != patch {
			return false
		}
	}
	return true
}

// Performs a `>` match with the given version bound. The match only
// succeeds if some version element in the bound is actually greater than
// that of the version.
func (version PackageVersion) matches_gt(bound *VersionBound) bool {
	if version.major != bound.major {
		return version.major > bound.major
	}
	minor, ok := bound.minor.Get()
	if !ok {
		return false
	}
	if version.minor != minor {
		return version.minor > minor
	}
	patch, ok := bound.patch.Get()
	if !ok {
		return false
	}
	if version.patch != patch {
		return version.patch > patch
	}
	return false
}

// Performs a `<` match with the given version bound. The match only
// succeeds if some version element in the bound is actually less than that
// of the version.
func (version PackageVersion) matches_lt(bound *VersionBound) bool {
	if version.major != bound.major {
		return version.major < bound.major
	}
	minor, ok := bound.minor.Get()
	if !ok {
		return false
	}
	if version.minor != minor {
		return version.minor < minor
	}
	patch, ok := bound.patch.Get()
	if !ok {
		return false
	}
	if version.patch != patch {
		return version.patch < patch
	}
	return false
}

// Performs a `>=` match with the given versions. The match succeeds when
// either a `==` or `>` match does.
func (version PackageVersion) matches_ge(bound *VersionBound) bool {
	return version.matches_eq(bound) || version.matches_gt(bound)
}

// Performs a `<=` match with the given versions. The match succeeds when
// either a `==` or `<` match does.
func (version PackageVersion) matches_le(bound *VersionBound) bool {
	return version.matches_eq(bound) || version.matches_lt(bound)
}

func CurrentCompilerVersion() PackageVersion {
	return PackageVersion{
		major: 0,
		minor: 13,
		patch: 1,
	}
}

// A version bound for compatibility specification.
type VersionBound struct {
	// The bounds's major version.
	major uint32
	// The bounds's minor version.
	minor option.Option[uint32]
	// The bounds's patch version. Can only be present if minor is too.
	patch option.Option[uint32]
}

// FromStr for VersionBound
func ParseVersionBound(str string) (*VersionBound, error) {
	parts := strings.Split(str, ".")
	kinds := []string{"major", "minor", "patch"}
	var major uint32
	var minor, patch option.Option[uint32]
	for i, kind := range kinds {
		if len(parts) <= i {
			switch kind {
			case "minor", "patch":
				continue // skip optional
			}
			return nil, errors.Errorf("version number is missing %v version", kind)
		}
		part := parts[i]
		x, err := strconv.ParseUint(part, 10, 32)
		if err != nil {
			return nil, errors.Wrapf(err, "`%v` is not a valid %v version", part, kind)
		}
		switch kind {
		case "major":
			major = uint32(x)
		case "minor":
			minor = option.Some(uint32(x))
		case "patch":
			patch = option.Some(uint32(x))
		}
	}
	if len(parts) > 3 {
		return nil, errors.Errorf("version number has unexpected components: `%v`", parts[3:])
	}
	return &VersionBound{
		major: major,
		minor: minor,
		patch: patch,
	}, nil
}

func (version *VersionBound) String() string {
	out := &strings.Builder{}
	fmt.Fprintf(out, "%d", version.major)
	if minor, ok := version.minor.Get(); ok {
		fmt.Fprintf(out, "%d", minor)
	}
	if patch, ok := version.patch.Get(); ok {
		fmt.Fprintf(out, "%d", patch)
	}
	return out.String()
}

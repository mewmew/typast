package syntax

import (
	"reflect"
	"testing"
)

func TestVersionVersionMatch(t *testing.T) {
	v1_1_1 := mustParsePackageVersion("1.1.1")

	// equal
	if !(v1_1_1.matches_eq(mustParseVersionBound("1"))) {
		t.Errorf("expected 1.1.1 = 1")
	}
	if !(v1_1_1.matches_eq(mustParseVersionBound("1.1"))) {
		t.Errorf("expected 1.1.1 = 1.1")
	}
	if !(!v1_1_1.matches_eq(mustParseVersionBound("1.2"))) {
		t.Errorf("expected !(1.1.1 = 1.2)")
	}

	// greater than
	if !(!v1_1_1.matches_gt(mustParseVersionBound("1"))) {
		t.Errorf("expected 1.1.1 > 1")
	}
	if !(v1_1_1.matches_gt(mustParseVersionBound("1.0"))) {
		t.Errorf("expected 1.1.1 > 1.0")
	}
	if !(!v1_1_1.matches_gt(mustParseVersionBound("1.1"))) {
		t.Errorf("expected !(1.1.1 > 1.1)")
	}

	// less than
	if !(!v1_1_1.matches_lt(mustParseVersionBound("1"))) {
		t.Errorf("expected !(1.1.1 < 1)")
	}
	if !(!v1_1_1.matches_lt(mustParseVersionBound("1.1"))) {
		t.Errorf("expected !(1.1.1 < 1.1)")
	}
	if !(v1_1_1.matches_lt(mustParseVersionBound("1.2"))) {
		t.Errorf("expected 1.1.1 < 1.2")
	}
}

func TestMinimalManifest(t *testing.T) {
	const content = `
[package]
name = "package"
version = "0.1.0"
entrypoint = "src/lib.typ"
`
	got := mustParsePackageManifestFromToml(content)
	want := &PackageManifest{
		Pkg: &PackageInfo{
			Name:       "package",
			Version:    PackageVersion{major: 0, minor: 1, patch: 0},
			Entrypoint: "src/lib.typ",
		},
	}
	if !reflect.DeepEqual(want, got) {
		t.Errorf("manifest mismatch; expected %#v, got %#v", want, got)
	}
}

// ### [ Utility functions ] ###################################################

func mustParsePackageVersion(str string) PackageVersion {
	version, err := ParsePackageVersion(str)
	if err != nil {
		panic(err)
	}
	return version
}

func mustParseVersionBound(str string) *VersionBound {
	version, err := ParseVersionBound(str)
	if err != nil {
		panic(err)
	}
	return version
}

func mustParsePackageManifestFromToml(content string) *PackageManifest {
	manifest, err := ParsePackageManifestFromToml(content)
	if err != nil {
		panic(err)
	}
	return manifest
}

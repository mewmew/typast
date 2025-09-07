package syntax

import (
	"reflect"
	"testing"
)

func TestVersionVersionMatch(t *testing.T) {
	v1_1_1 := mustParsePackageVersion("1.1.1")

	// equal
	if !(v1_1_1.MatchesEQ(mustParseVersionBound("1"))) {
		t.Errorf("expected 1.1.1 = 1")
	}
	if !(v1_1_1.MatchesEQ(mustParseVersionBound("1.1"))) {
		t.Errorf("expected 1.1.1 = 1.1")
	}
	if !(!v1_1_1.MatchesEQ(mustParseVersionBound("1.2"))) {
		t.Errorf("expected !(1.1.1 = 1.2)")
	}

	// greater than
	if !(!v1_1_1.MatchesGT(mustParseVersionBound("1"))) {
		t.Errorf("expected 1.1.1 > 1")
	}
	if !(v1_1_1.MatchesGT(mustParseVersionBound("1.0"))) {
		t.Errorf("expected 1.1.1 > 1.0")
	}
	if !(!v1_1_1.MatchesGT(mustParseVersionBound("1.1"))) {
		t.Errorf("expected !(1.1.1 > 1.1)")
	}

	// less than
	if !(!v1_1_1.MatchesLT(mustParseVersionBound("1"))) {
		t.Errorf("expected !(1.1.1 < 1)")
	}
	if !(!v1_1_1.MatchesLT(mustParseVersionBound("1.1"))) {
		t.Errorf("expected !(1.1.1 < 1.1)")
	}
	if !(v1_1_1.MatchesLT(mustParseVersionBound("1.2"))) {
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
			Version:    PackageVersion{Major: 0, Minor: 1, Patch: 0},
			Entrypoint: "src/lib.typ",
		},
	}
	if !reflect.DeepEqual(want, got) {
		t.Errorf("manifest mismatch; expected %#v, got %#v", want, got)
	}
}

func TestParsePackageSpec(t *testing.T) {
	golden := []struct {
		input string
		want  PackageSpec
	}{
		{
			input: "@preview/smartaref:0.1.0",
			want: PackageSpec{
				Namespace: "preview",
				Name:      "smartaref",
				Version:   PackageVersion{Major: 0, Minor: 1, Patch: 0},
			},
		},
		{
			input: "@local/foo:1.2.3",
			want: PackageSpec{
				Namespace: "local",
				Name:      "foo",
				Version:   PackageVersion{Major: 1, Minor: 2, Patch: 3},
			},
		},
	}
	for _, g := range golden {
		_got := mustParsePackageSpec(g.input)
		got := *_got
		if g.want != got {
			t.Errorf("package spec mismatch; expected %#v, got %#v", g.want, got)
		}
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

func mustParsePackageSpec(str string) *PackageSpec {
	spec, err := ParsePackageSpec(str)
	if err != nil {
		panic(err)
	}
	return spec
}

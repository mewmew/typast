package syntax

import (
	"testing"
)

func TestSet(t *testing.T) {
	set := NewSyntaxSet().add(SyntaxKindAnd).add(SyntaxKindOr)
	if !set.contains(SyntaxKindAnd) {
		t.Errorf("expected set %v to contain %v", set, SyntaxKindAnd)
	}
	if !set.contains(SyntaxKindOr) {
		t.Errorf("expected set %v to contain %v", set, SyntaxKindOr)
	}
	if set.contains(SyntaxKindNot) {
		t.Errorf("expected set %v to not contain %v", set, SyntaxKindNot)
	}
}

package syntax

import (
	"testing"
)

func TestSet(t *testing.T) {
	set := NewSyntaxSet().Add(SyntaxKindAnd).Add(SyntaxKindOr)
	if !set.Contains(SyntaxKindAnd) {
		t.Errorf("expected set %v to contain %v", set, SyntaxKindAnd)
	}
	if !set.Contains(SyntaxKindOr) {
		t.Errorf("expected set %v to contain %v", set, SyntaxKindOr)
	}
	if set.Contains(SyntaxKindNot) {
		t.Errorf("expected set %v to not contain %v", set, SyntaxKindNot)
	}
}

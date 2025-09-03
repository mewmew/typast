package syntax

import (
	"reflect"
	"testing"
)

func TestExprDefault(t *testing.T) {
	// round-trip test.
	want := Expr_default()
	node := want.to_untyped()
	got, ok := SyntaxNode_cast[Expr](node).Get()
	if !ok {
		t.Fatalf("expected Some, got None")
	}
	if !reflect.DeepEqual(want, got) {
		t.Errorf("round-trip mismatch; expected %v, got %v", want, got)
	}
}

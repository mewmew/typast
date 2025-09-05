// Inspiration for the Typst walker was taken from Go fix.

// Package walk implements a Typst CST walker.
package cstwalk

import (
	"fmt"

	"github.com/mewmew/typast/typst/syntax"
)

// Walk walks the Typst CST in depth-first order; invoking visit recursively for
// each non-nil child of root. If visit returns false, the walk is terminated.
func Walk(root any, visit func(n any) bool) {
	visited := make(map[any]bool)
	walk(root, visit, visited)
}

// walk walks the Typst CST in depth-first order; invoking visit recursively for
// each non-nil child of root. If visit returns false, the walk is terminated.
// Visited tracks visited nodes.
func walk(root any, visit func(n any) bool, visited map[any]bool) {
	if visited[root] {
		return
	}
	visited[root] = true
	if !visit(root) {
		return
	}
	switch root := root.(type) {
	// pointer to pointer to struct.
	case **syntax.SyntaxNode:
		walk(*root, visit, visited)
	case **syntax.LeafNode:
		walk(*root, visit, visited)
	case **syntax.InnerNode:
		walk(*root, visit, visited)
	case **syntax.ErrorNode:
		walk(*root, visit, visited)

	// pointer to struct (with value receiver).
	// nothing to do

	// pointer to interface.
	case *syntax.Repr:
		walk(*root, visit, visited)

	// pointer to struct.
	case *syntax.SyntaxNode:
		walk(&root.Repr, visit, visited)
	case *syntax.LeafNode:
		// nothing to do.
	case *syntax.InnerNode:
		for i := range root.Children {
			walk(&root.Children[i], visit, visited)
		}
	case *syntax.ErrorNode:
		// nothing to do.

	// struct (with value receiver).
	// nothing to do.

	// interface.
	case syntax.Repr:
		walkRepr(root, visit, visited)
	default:
		panic(fmt.Errorf("support for Typst CST node type %T not yet implemented", root))
	}
}

// walkRepr walks the Typst CST in depth-first order; invoking visit recursively
// for each non-nil child of root. If visit returns false, the walk is
// terminated. Visited tracks visited nodes.
func walkRepr(root syntax.Repr, visit func(n any) bool, visited map[any]bool) {
	switch root := root.(type) {
	case *syntax.LeafNode:
		walk(root, visit, visited)
	case *syntax.InnerNode:
		walk(root, visit, visited)
	case *syntax.ErrorNode:
		walk(root, visit, visited)
	default:
		panic(fmt.Errorf("support for Typst CST node type %T not yet implemented", root))
	}
}

package syntax

import "github.com/mewmew/typast/internal/option"

type Range struct {
	Start uint64
	End   uint64
}

// Defines a range in a file.
//
// This is used throughout the compiler to track which source section an
// element stems from or an error applies to.
//
//   - The [`.id()`](Self::id) function can be used to get the `FileId` for the
//     span and, by extension, its file system path.
//   - The `WorldExt::range` function can be used to map the span to a
//     `Range<usize>`.
//
// This type takes up 8 bytes and is copyable and null-optimized (i.e.
// `Option<Span>` also takes 8 bytes).
//
// Spans come in two flavors: Numbered spans and raw range spans. The
// `WorldExt::range` function automatically handles both cases, yielding a
// `Range<usize>`.
//
// # Numbered spans
// Typst source files use _numbered spans._ Rather than using byte ranges,
// which shift a lot as you type, each AST node gets a unique number.
//
// During editing, the span numbers stay mostly stable, even for nodes behind
// an insertion. This is not true for simple ranges as they would shift. Spans
// can be used as inputs to memoized functions without hurting cache
// performance when text is inserted somewhere in the document other than the
// end.
//
// Span ids are ordered in the syntax tree to enable quickly finding the node
// with some id:
//   - The id of a parent is always smaller than the ids of any of its children.
//   - The id of a node is always greater than any id in the subtrees of any left
//     sibling and smaller than any id in the subtrees of any right sibling.
//
// # Raw range spans
// Non Typst-files use raw ranges instead of numbered spans. The maximum
// encodable value for start and end is 2^23. Larger values will be saturated.
type Span struct {
	fileId option.Option[FileId]
	_range option.Option[Range]
}

// A value with a span locating it in the source code.
type Spanned[T any] struct {
	// The spanned value.
	v T
	// The value's location in source code.
	span Span
}

// Create a new instance from a value and its span.
//
// new
func NewSpanned[T any](v T, span Span) *Spanned[T] {
	return &Spanned[T]{
		v:    v,
		span: span,
	}
}

// Source file management.

package syntax

import (
	"fmt"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
)

// --- [ Source ] --------------------------------------------------------------

// A source file.
//
// All line and column indices start at zero, just like byte indices. Only for
// user-facing display, you should add 1 to them.
//
// Values of this type are cheap to clone and hash.
type Source struct {
	// The id of the source file.
	id FileID
	// The root node of the file's untyped syntax tree.
	root *SyntaxNode
	// An acceleration structure for conversion of UTF-8, UTF-16 and
	// line/column indices.
	lines *Lines
}

// Create a new source file.
//
// new
func NewSource(id FileID, text string) *Source {
	root := Parse(text)
	if err := root.numberize(id, SpanFULL); err != nil {
		panic(err)
	}
	return &Source{
		id:    id,
		root:  root,
		lines: NewLines(text),
	}
}

// Create a source file without a real id and path, usually for testing.
//
// detached
func Source_detached(text string) *Source {
	id := NewFileID(option.None[*PackageSpec](), NewVirtualPath("main.typ"))
	return NewSource(id, text)
}

// The whole source as a string slice.
func (src *Source) text() string {
	return src.lines.text
}

// Fully replace the source text.
//
// This performs a naive (suffix/prefix-based) diff of the old and new text
// to produce the smallest single edit that transforms old into new and
// then calls [`edit`](Self::edit) with it.
//
// Returns the range in the new source that was ultimately reparsed.
func (src *Source) replace(new string) ranges.Range {
	_replacement_range, ok := src.lines.replacement_range(new).Get()
	if !ok {
		return ranges.NewRange(0, 0)
	}
	_prefix := _replacement_range.prefix
	_suffix := _replacement_range.suffix

	old := src.text()
	replace := ranges.NewRange(uint64(_prefix), uint64(uint(len(old))-_suffix))
	new_range := ranges.NewRange(uint64(_prefix), uint64(uint(len(new))-_suffix))
	with := new[new_range.Start:new_range.End]
	return src.edit(replace, with)
}

// Edit the source file by replacing the given range.
//
// Returns the range in the new source that was ultimately reparsed.
//
// The method panics if the `replace` range is out of bounds.
func (src *Source) edit(replace ranges.Range, with string) ranges.Range {
	// Update the text and lines.
	src.lines.edit(replace, with)

	// Incrementally reparse the replaced range.
	return reparse(src.root, src.lines.text, replace, uint(len(with)))
}

// TODO: uncomment Source_find

// Find the node with the given span.
//
// Returns `None` if the span does not point into this source file.
//
// find
func Source_find(src *Source, span Span) option.Option[*LinkedNode] {
	return NewLinkedNode(src.root).find(span)
}

// TODO: uncomment Source._range

// Get the byte range for the given span in this file.
//
// Returns `None` if the span does not point into this source file.
//
// Typically, it's easier to use `WorldExt::range` instead.
//
// range
//func (src *Source) _range(span Span) option.Option[ranges.Range] {
//	link, ok := Source_find(src, span).Get()
//	if !ok {
//		panic(fmt.Sprintf("unable to find node with span %v", span))
//	}
//	return option.Some(link._range())
//}

func (src *Source) String() string {
	return fmt.Sprintf("Source(%v)", src.id.VPath())
}

// --- [/ Source ] -------------------------------------------------------------

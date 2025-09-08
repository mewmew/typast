// Source file management.

package syntax

import (
	"fmt"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
)

// --- [ Source ] --------------------------------------------------------------

// Source represents a source file.
//
// All line and column indices start at zero, just like byte indices. For
// user-facing display, add 1 to them.
//
// Values of this type are cheap to clone and hash.
type Source struct {
	// The ID of the source file.
	ID FileID
	// The root node of the file's untyped syntax tree.
	Root *SyntaxNode
	// An acceleration structure for conversion of UTF-8, UTF-16 and
	// line/column indices.
	Lines *Lines
}

// NewSource creates a new source file from an ID and text.
//
// It panics if the text cannot be properly parsed.
func NewSource(id FileID, text string) *Source {
	root := Parse(text)
	if err := root.numberize(id, SpanFull); err != nil {
		panic(err)
	}
	return &Source{
		ID:    id,
		Root:  root,
		Lines: NewLines(text),
	}
}

// NewDetachedSource creates a source file without a real ID and path,
// usually for testing.
//
// It panics if the text cannot be properly parsed.
func NewDetachedSource(text string) *Source {
	id := NewFileID(option.None[*PackageSpec](), NewVirtualPath("main.typ"))
	return NewSource(id, text)
}

// Text returns the whole source as a string.
func (s *Source) Text() string {
	return s.Lines.Text
}

// Replace fully replaces the source text.
//
// It performs a naive (suffix/prefix-based) diff of the old and new text to
// produce the smallest single edit that transforms the old source into the new one.
// The method then calls Edit with this change.
//
// Replace returns the range in the new source that was ultimately reparsed.
func (s *Source) Replace(new string) ranges.Range {
	replacementRange, ok := s.Lines.ReplacementRange(new)
	if !ok {
		return ranges.NewRange(0, 0)
	}
	prefix := replacementRange.Prefix
	suffix := replacementRange.Suffix

	old := s.Text()
	replace := ranges.NewRange(uint64(prefix), uint64(uint(len(old))-suffix))
	newRange := ranges.NewRange(uint64(prefix), uint64(uint(len(new))-suffix))
	with := new[newRange.Start:newRange.End]
	return s.Edit(replace, with)
}

// Edit edits the source file by replacing the given range.
//
// It returns the range in the new source that was ultimately reparsed.
//
// This method panics if the replace range is out of bounds.
func (s *Source) Edit(replace ranges.Range, with string) ranges.Range {
	// Update the text and lines.
	s.Lines.Edit(replace, with)

	// Incrementally reparse the replaced range.
	return reparse(s.Root, s.Lines.Text, replace, uint(len(with)))
}

// Find finds the node with the given span.
//
// It returns None if the span does not point into this source file.
func (s *Source) Find(span Span) option.Option[*LinkedNode] {
	return NewLinkedNode(s.Root).find(span)
}

// Range returns the byte range for the given span in this file.
//
// It returns false if the span does not point into this source file.
//
// Typically, it's easier to use `WorldExt::Range` instead.
func (s *Source) Range(span Span) (ranges.Range, bool) {
	link, ok := s.Find(span).Get()
	if !ok {
		return ranges.Range{}, false
	}
	return link.Range(), true
}

// String returns a string representation of the Source.
func (s *Source) String() string {
	return fmt.Sprintf("Source(%v)", s.ID.VPath())
}

// --- [/ Source ] -------------------------------------------------------------

package syntax

import (
	"iter"

	"github.com/mewmew/typast/internal/ranges"
	"github.com/pkg/errors"
)

// --- [ Span ] ----------------------------------------------------------------

// A Span defines a range in a file.
//
// It's used to track which source section an element stems from or
// an error applies to.
//
// This type takes up 8 bytes and is copyable.
//
// Data layout:
// | 16 bits file id | 48 bits number |
//
// Number =
//   - 1 means detached
//   - 2..2^47-1 is a numbered span
//   - 2^47..2^48-1 is a raw range span. To retrieve it, you must subtract
//     `rangeBase` and then use shifting/bitmasking to extract the
//     components.
type Span uint64

const (
	numberBits     = 48
	fileIDShift    = numberBits
	numberMask     = (uint64(1) << numberBits) - 1
	rangeBase      = uint64(1) << 47
	rangePartBits  = 23
	rangePartShift = rangePartBits
	rangePartMask  = (uint64(1) << rangePartBits) - 1
	spanDetached   = 1
	maxRangePart   = uint64(1) << rangePartBits
)

var (
	// The full range of numbers available for source file span numbering.
	SpanFull = ranges.NewRange(2, rangeBase)
)

// NewDetachedSpan creates a span that does not point into any file.
func NewDetachedSpan() Span {
	return Span(spanDetached)
}

// NewSpanFromNumber creates a new span from a file ID and a number.
//
// It returns true if the number is within the valid range.
func NewSpanFromNumber(id FileID, number uint64) (Span, bool) {
	if number < SpanFull.Start || number >= SpanFull.End {
		return 0, false
	}
	s := NewSpanPacked(id, number)
	return s, true
}

// NewSpanFromRange creates a new span from a raw byte range.
// If a range part exceeds the maximum value (2^23), it is saturated.
func NewSpanFromRange(id FileID, byteRange ranges.Range) Span {
	start := min(byteRange.Start, maxRangePart)
	end := min(byteRange.End, maxRangePart)
	number := (start << rangePartShift) | end
	return NewSpanPacked(id, rangeBase+number)
}

// NewSpanFromUint64 constructs a span from a raw number.
func NewSpanFromUint64(v uint64) Span {
	return Span(v)
}

// NewSpanPacked packs a file ID and the low bits into a span.
func NewSpanPacked(id FileID, low uint64) Span {
	bits := (uint64(id.Uint16()) << fileIDShift) | low
	if bits == 0 {
		panic("file ID should be non-zero")
	}
	return Span(bits)
}

// IsDetached returns whether the span is detached.
func (s Span) IsDetached() bool {
	return uint64(s) == spanDetached
}

// ID returns the ID of the file the span points into.
// It returns true if the span is not detached.
func (s Span) ID() (FileID, bool) {
	fileIDBits := uint16(uint64(s) >> fileIDShift)
	if fileIDBits == 0 {
		return 0, false
	}
	return FileIDFromUint16(fileIDBits), true
}

// Number returns the unique number of the span within its source.
func (s Span) Number() uint64 {
	return uint64(s) & numberMask
}

// RawRange extracts a raw byte range from the span.
// It returns true if the span is a raw range span.
func (s Span) RawRange() (ranges.Range, bool) {
	number := s.Number()
	if number < rangeBase {
		return ranges.Range{}, false
	}

	number -= rangeBase
	start := number >> rangePartShift
	end := number & rangePartMask
	return ranges.NewRange(start, end), true
}

// Uint64 extracts the raw underlying number.
func (s Span) Uint64() uint64 {
	return uint64(s)
}

// Or returns other if s is detached and s otherwise.
func (s Span) Or(other Span) Span {
	if s.IsDetached() {
		return other
	}
	return s
}

// FindFirstNonDetached finds the first non-detached span in the iterator.
func FindFirstNonDetached(spans iter.Seq[Span]) Span {
	for span := range spans {
		if !span.IsDetached() {
			return span
		}
	}
	return NewDetachedSpan()
}

// ResolvePath resolves a file location relative to this span's source.
func (s Span) ResolvePath(path string) (FileID, error) {
	id, ok := s.ID()
	if !ok {
		return 0, errors.New("cannot access file system from a detached span")
	}
	return id.Join(path), nil
}

// --- [ Spanned ] -------------------------------------------------------------

// A Spanned value with a span locating it in the source code.
type Spanned[T any] struct {
	// The spanned value.
	Value T
	// The value's location in source code.
	Span Span
}

// NewSpanned creates a new instance from a value and its span.
func NewSpanned[T any](v T, span Span) Spanned[T] {
	return Spanned[T]{
		Value: v,
		Span:  span,
	}
}

// Map maps the value using a function.
func (s Spanned[T]) Map(f func(T) T) Spanned[T] {
	return NewSpanned(f(s.Value), s.Span)
}

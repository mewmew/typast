package syntax

import (
	"iter"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
	"github.com/pkg/errors"
)

// --- [ Span ] ----------------------------------------------------------------

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
type Span uint64 // NonZeroU64

// The full range of numbers available for source file span numbering.
var SpanFULL = ranges.NewRange(2, 1<<47)

// The value reserved for the detached span.
const SpanDETACHED uint64 = 1

// Data layout:
// | 16 bits file id | 48 bits number |
//
// Number =
//   - 1 means detached
//   - 2..2^47-1 is a numbered span
//   - 2^47..2^48-1 is a raw range span. To retrieve it, you must subtract
//     `RANGE_BASE` and then use shifting/bitmasking to extract the
//     components.
const (
	NUMBER_BITS             = 48
	FILE_ID_SHIFT           = NUMBER_BITS
	NUMBER_MASK      uint64 = (1 << NUMBER_BITS) - 1
	RANGE_PART_BITS         = 23
	RANGE_PART_SHIFT        = RANGE_PART_BITS
	RANGE_PART_MASK  uint64 = (1 << RANGE_PART_BITS) - 1
)

var (
	RANGE_BASE uint64 = SpanFULL.End
)

// Create a span that does not point into any file.
//
// detached
func Span_detached() Span {
	return Span(SpanDETACHED)
}

// Create a new span from a file id and a number.
//
// Returns `None` if `number` is not contained in `FULL`.
//
// from_number
func Span_from_number(id FileId, number uint64) option.Option[Span] {
	if number < SpanFULL.Start || number >= SpanFULL.End {
		return option.None[Span]()
	}
	return option.Some(Span_pack(id, number))
}

// Create a new span from a raw byte range instead of a span number.
//
// If one of the range's parts exceeds the maximum value (2^23), it is
// saturated.
//
// from_range
func Span_from_range(id FileId, _range ranges.Range) Span {
	_max := uint64(1 << RANGE_PART_BITS)
	start := min(_range.Start, _max)
	end := min(_range.End, _max)
	number := (start << RANGE_PART_SHIFT) | end
	return Span_pack(id, RANGE_BASE+number)
}

// Construct from a raw number.
//
// Should only be used with numbers retrieved via
// [`into_raw`](Self::into_raw). Misuse may results in panics, but no
// unsafety.
//
// # NonZeroU64
//
// from_raw
func Span_from_raw(v uint64) Span {
	return Span(v)
}

// Pack a file ID and the low bits into a span.
//
// pack
func Span_pack(id FileId, low uint64) Span {
	bits := (uint64(id.into_raw()) << FILE_ID_SHIFT) | low

	// The file ID is non-zero.
	if bits == 0 {
		panic("file ID should be non-zero")
	}
	return Span(bits)
}

// Whether the span is detached.
func (span Span) is_detached() bool {
	return uint64(span) == SpanDETACHED
}

// The id of the file the span points into.
//
// Returns `None` if the span is detached.
func (span Span) id() option.Option[FileId] {
	// Detached span has only zero high bits, so it will trigger the
	// `None` case.
	file_id_bits := uint16(uint64(span) >> FILE_ID_SHIFT)
	if file_id_bits == 0 {
		return option.None[FileId]()
	}
	return option.Some(FileId_from_raw(file_id_bits))
}

// The unique number of the span within its [`Source`](crate::Source).
func (span Span) number() uint64 {
	return uint64(span) & NUMBER_MASK
}

// Extract a raw byte range from the span, if it is a raw range span.
//
// Typically, you should use `WorldExt::range` instead.
func (span Span) _range() option.Option[ranges.Range] {
	number := span.number() - RANGE_BASE
	if number < 0 {
		return option.None[ranges.Range]()
	}
	start := number >> RANGE_PART_SHIFT
	end := number & RANGE_PART_MASK
	return option.Some(ranges.NewRange(start, end))
}

// Extract the raw underlying number.
//
// NonZeroU64
func (span Span) into_raw() uint64 {
	return uint64(span)
}

// Return `other` if `self` is detached and `self` otherwise.
func (span Span) or(other Span) Span {
	if span.is_detached() {
		return other
	} else {
		return span
	}
}

// Find the first non-detached span in the iterator.
//
// find
func Span_find(spans iter.Seq[Span]) Span {
	for span := range spans {
		if !span.is_detached() {
			return span
		}
	}
	return Span_detached()
}

// Resolve a file location relative to this span's source.
func (span Span) resolve_path(path string) (FileId, error) {
	id, ok := span.id().Get()
	if !ok {
		return 0, errors.New("cannot access file system from here")
	}
	return id.join(path), nil
}

// --- [/ Span ] ---------------------------------------------------------------

// --- [ Spanned ] -------------------------------------------------------------

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
func NewSpanned[T any](v T, span Span) Spanned[T] {
	return Spanned[T]{
		v:    v,
		span: span,
	}
}

// Convert from `&Spanned<T>` to `Spanned<&T>`
//func (spanned *Spanned[T]) as_ref() Spanned[*T] {
//	return Spanned{
//		v:    &spanned.v,
//		span: spanned.span,
//	}
//}

// Map the value using a function.
//
// map
func Spanned_map[T, U any](spanned Spanned[T], f func(T) U) Spanned[U] {
	return Spanned[U]{
		v:    f(spanned.v),
		span: spanned.span,
	}
}

// --- [/ Spanned ] ------------------------------------------------------------

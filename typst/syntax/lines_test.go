package syntax

import (
	"reflect"
	"testing"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
)

const TEST = "ä\tcde\nf💛g\r\nhi\rjkl"

func TestSourceFileNew(t *testing.T) {
	want := []Line{
		{byte_idx: 0, utf16_idx: 0},
		{byte_idx: 7, utf16_idx: 6},
		{byte_idx: 15, utf16_idx: 12},
		{byte_idx: 18, utf16_idx: 15},
	}
	got := NewLines(TEST)
	if !reflect.DeepEqual(want, got.lines) {
		t.Errorf("lines mismatch; expected %v, got %v", want, got.lines)
	}
}

func TestSourceFilePosToLine(t *testing.T) {
	golden := []struct {
		byte_idx uint
		want     option.Option[uint]
	}{
		{byte_idx: 0, want: option.Some[uint](0)},
		{byte_idx: 2, want: option.Some[uint](0)},
		{byte_idx: 6, want: option.Some[uint](0)},
		{byte_idx: 7, want: option.Some[uint](1)},
		{byte_idx: 8, want: option.Some[uint](1)},
		{byte_idx: 12, want: option.Some[uint](1)},
		{byte_idx: 21, want: option.Some[uint](3)},
		{byte_idx: 22, want: option.None[uint]()},
	}
	l := NewLines(TEST)
	for _, g := range golden {
		want, ok1 := g.want.Get()
		got, ok2 := l.byte_to_line(g.byte_idx).Get()
		if ok1 != ok2 {
			t.Errorf("option mismatch (for byte_idx=%d); expected %v, got %v", g.byte_idx, ok1, ok2)
			continue
		}
		if want != got {
			t.Errorf("line mismatch (for byte_idx=%d); expected %v, got %v", g.byte_idx, want, got)
		}
	}
}

func TestSourceFilePosToColumn(t *testing.T) {
	golden := []struct {
		byte_idx uint
		want     option.Option[uint]
	}{
		{byte_idx: 0, want: option.Some[uint](0)},
		{byte_idx: 2, want: option.Some[uint](1)},
		{byte_idx: 6, want: option.Some[uint](5)},
		{byte_idx: 7, want: option.Some[uint](0)},
		{byte_idx: 8, want: option.Some[uint](1)},
		{byte_idx: 12, want: option.Some[uint](2)},
	}
	l := NewLines(TEST)
	for _, g := range golden {
		want, ok1 := g.want.Get()
		got, ok2 := l.byte_to_column(g.byte_idx).Get()
		if ok1 != ok2 {
			t.Errorf("option mismatch (for byte_idx=%d); expected %v, got %v", g.byte_idx, ok1, ok2)
			continue
		}
		if want != got {
			t.Errorf("column mismatch (for byte_idx=%d); expected %v, got %v", g.byte_idx, want, got)
		}
	}
}

func TestSourceFileUtf16(t *testing.T) {
	roundtrip := func(l *Lines, byte_idx uint, utf16_idx uint) {
		middle := l.byte_to_utf16(byte_idx).MustGet()
		result := l.utf16_to_byte(middle).MustGet()
		if middle != utf16_idx {
			t.Errorf("roundtrip utf-16 index mismatch; expected %v, got %v", utf16_idx, middle)
		}
		if result != byte_idx {
			t.Errorf("roundtrip byte index mismatch; expected %v, got %v", byte_idx, result)
		}
	}

	l := NewLines(TEST)
	roundtrip(l, 0, 0)
	roundtrip(l, 2, 1)
	roundtrip(l, 3, 2)
	roundtrip(l, 8, 7)
	roundtrip(l, 12, 9)
	roundtrip(l, 21, 18)

	// Check for None result.
	const byte_idx = 22
	if _, ok := l.byte_to_utf16(byte_idx).Get(); ok {
		t.Errorf("result mismatch (for byte_idx=%d); expected None, got Some", byte_idx)
	}
	const utf16_idx = 19
	if _, ok := l.utf16_to_byte(utf16_idx).Get(); ok {
		t.Errorf("result mismatch (for utf16_idx=%d); expected None, got Some", utf16_idx)
	}
}

func TestSourceFileRoundtrip(t *testing.T) {
	roundtrip := func(l *Lines, byte_idx uint) {
		line := l.byte_to_line(byte_idx).MustGet()
		column := l.byte_to_column(byte_idx).MustGet()
		result := l.line_column_to_byte(line, column).MustGet()
		if result != byte_idx {
			t.Errorf("roundtrip (line, col) to byte index mismatch; expected %v, got %v", byte_idx, result)
		}
	}

	l := NewLines(TEST)
	roundtrip(l, 0)
	roundtrip(l, 7)
	roundtrip(l, 12)
	roundtrip(l, 21)
}

func TestSourceFileEdit(t *testing.T) {
	// This tests only the non-parser parts. The reparsing itself is
	// tested separately.
	test := func(prev string, _range ranges.Range, with, after string) {
		reference := NewLines(after)

		edited := NewLines(prev)
		edited.edit(_range, with)
		if edited.text != reference.text {
			t.Errorf("edited text mismatch; expected %q, got %q", reference.text, edited.text)
		}
		if !reflect.DeepEqual(edited.lines, reference.lines) {
			t.Errorf("edited lines mismatch; expected %v, got %v", reference.lines, edited.lines)
		}

		replaced := NewLines(prev)
		new_prev := string_replace_range(prev, _range, with)
		replaced.replace(new_prev)
		if replaced.text != reference.text {
			t.Errorf("replaced text mismatch; expected %q, got %q", reference.text, replaced.text)
		}
		if !reflect.DeepEqual(replaced.lines, reference.lines) {
			t.Errorf("replaced lines mismatch; expected %v, got %v", reference.lines, replaced.lines)
		}
	}

	// Test inserting at the beginning.
	test("abc\n", ranges.NewRange(0, 0), "hi\n", "hi\nabc\n")
	test("\nabc", ranges.NewRange(0, 0), "hi\r", "hi\r\nabc")

	// Test editing in the middle.
	test(TEST, ranges.NewRange(4, 16), "❌", "ä\tc❌i\rjkl")

	// Test appending.
	test("abc\ndef", ranges.NewRange(7, 7), "hi", "abc\ndefhi")
	test("abc\ndef\n", ranges.NewRange(8, 8), "hi", "abc\ndef\nhi")

	// Test appending with adjoining \r and \n.
	test("abc\ndef\r", ranges.NewRange(8, 8), "\nghi", "abc\ndef\r\nghi")

	// Test removing everything.
	test(TEST, ranges.NewRange(0, 21), "", "")
}

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
		{ByteIdx: 0, Utf16Idx: 0},
		{ByteIdx: 7, Utf16Idx: 6},
		{ByteIdx: 15, Utf16Idx: 12},
		{ByteIdx: 18, Utf16Idx: 15},
	}
	got := NewLines(TEST)
	if !reflect.DeepEqual(want, got.Lines) {
		t.Errorf("lines mismatch; expected %v, got %v", want, got.Lines)
	}
}

func TestSourceFilePosToLine(t *testing.T) {
	golden := []struct {
		ByteIdx uint
		want    option.Option[uint]
	}{
		{ByteIdx: 0, want: option.Some[uint](0)},
		{ByteIdx: 2, want: option.Some[uint](0)},
		{ByteIdx: 6, want: option.Some[uint](0)},
		{ByteIdx: 7, want: option.Some[uint](1)},
		{ByteIdx: 8, want: option.Some[uint](1)},
		{ByteIdx: 12, want: option.Some[uint](1)},
		{ByteIdx: 21, want: option.Some[uint](3)},
		{ByteIdx: 22, want: option.None[uint]()},
	}
	l := NewLines(TEST)
	for _, g := range golden {
		want, ok1 := g.want.Get()
		got, ok2 := l.ByteToLine(g.ByteIdx)
		if ok1 != ok2 {
			t.Errorf("option mismatch (for ByteIdx=%d); expected %v, got %v", g.ByteIdx, ok1, ok2)
			continue
		}
		if want != got {
			t.Errorf("line mismatch (for ByteIdx=%d); expected %v, got %v", g.ByteIdx, want, got)
		}
	}
}

func TestSourceFilePosToColumn(t *testing.T) {
	golden := []struct {
		ByteIdx uint
		want    option.Option[uint]
	}{
		{ByteIdx: 0, want: option.Some[uint](0)},
		{ByteIdx: 2, want: option.Some[uint](1)},
		{ByteIdx: 6, want: option.Some[uint](5)},
		{ByteIdx: 7, want: option.Some[uint](0)},
		{ByteIdx: 8, want: option.Some[uint](1)},
		{ByteIdx: 12, want: option.Some[uint](2)},
	}
	l := NewLines(TEST)
	for _, g := range golden {
		want, ok1 := g.want.Get()
		got, ok2 := l.ByteToColumn(g.ByteIdx)
		if ok1 != ok2 {
			t.Errorf("option mismatch (for ByteIdx=%d); expected %v, got %v", g.ByteIdx, ok1, ok2)
			continue
		}
		if want != got {
			t.Errorf("column mismatch (for ByteIdx=%d); expected %v, got %v", g.ByteIdx, want, got)
		}
	}
}

func TestSourceFileUtf16(t *testing.T) {
	roundtrip := func(l *Lines, ByteIdx uint, Utf16Idx uint) {
		middle, _ := l.ByteToUtf16(ByteIdx)
		result, _ := l.Utf16ToByte(middle)
		if middle != Utf16Idx {
			t.Errorf("roundtrip utf-16 index mismatch; expected %v, got %v", Utf16Idx, middle)
		}
		if result != ByteIdx {
			t.Errorf("roundtrip byte index mismatch; expected %v, got %v", ByteIdx, result)
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
	const ByteIdx = 22
	if _, ok := l.ByteToUtf16(ByteIdx); ok {
		t.Errorf("result mismatch (for ByteIdx=%d); expected None, got Some", ByteIdx)
	}
	const Utf16Idx = 19
	if _, ok := l.Utf16ToByte(Utf16Idx); ok {
		t.Errorf("result mismatch (for Utf16Idx=%d); expected None, got Some", Utf16Idx)
	}
}

func TestSourceFileRoundtrip(t *testing.T) {
	roundtrip := func(l *Lines, ByteIdx uint) {
		line, _ := l.ByteToLine(ByteIdx)
		column, _ := l.ByteToColumn(ByteIdx)
		result, _ := l.LineColumnToByte(line, column)
		if result != ByteIdx {
			t.Errorf("roundtrip (line, col) to byte index mismatch; expected %v, got %v", ByteIdx, result)
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
		edited.Edit(_range, with)
		if edited.Text != reference.Text {
			t.Errorf("edited text mismatch; expected %q, got %q", reference.Text, edited.Text)
		}
		if !reflect.DeepEqual(edited.Lines, reference.Lines) {
			t.Errorf("edited lines mismatch; expected %v, got %v", reference.Lines, edited.Lines)
		}

		replaced := NewLines(prev)
		new_prev := replaceRange(prev, _range, with)
		replaced.Replace(new_prev)
		if replaced.Text != reference.Text {
			t.Errorf("replaced text mismatch; expected %q, got %q", reference.Text, replaced.Text)
		}
		if !reflect.DeepEqual(replaced.Lines, reference.Lines) {
			t.Errorf("replaced lines mismatch; expected %v, got %v", reference.Lines, replaced.Lines)
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

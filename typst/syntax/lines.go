package syntax

import (
	"fmt"
	"sort"
	"strings"
	"unicode/utf16"
	"unicode/utf8"

	"github.com/mewmew/typast/internal/ranges"
	"github.com/mewmew/typast/internal/scanner"
	"github.com/mewmew/typast/internal/stdx"
)

// Lines is a text buffer and metadata about lines.
//
// This is internally reference-counted and thus cheap to clone.
type Lines struct {
	// The text as a string slice.
	Text  string
	Lines []Line
}

// Line represents metadata about a line.
type Line struct {
	// The UTF-8 byte offset where the line starts.
	ByteIdx uint
	// The UTF-16 code unit offset where the line starts.
	Utf16Idx uint
}

// NewLines creates a new Lines buffer from the text and computes the line metadata.
func NewLines(text string) *Lines {
	var lines []Line
	lines = append(lines, Line{ByteIdx: 0, Utf16Idx: 0})
	lines = append(lines, linesFrom(0, 0, text)...)
	return &Lines{
		Text:  text,
		Lines: lines,
	}
}

// LenBytes returns the length of the file in UTF-8 encoded bytes.
func (l *Lines) LenBytes() uint {
	return uint(len(l.Text))
}

// LenUtf16 returns the length of the file in UTF-16 code units.
func (l *Lines) LenUtf16() uint {
	last := l.Lines[len(l.Lines)-1]
	return last.Utf16Idx + lenUtf16(l.Text[last.ByteIdx:])
}

// LenLines returns the length of the file in lines.
func (l *Lines) LenLines() uint {
	return uint(len(l.Lines))
}

// ByteToUtf16 returns the index of the UTF-16 code unit at the byte index.
//
// It returns false if the byte index is out of bounds.
func (l *Lines) ByteToUtf16(byteIdx uint) (uint, bool) {
	lineIdx, ok := l.ByteToLine(byteIdx)
	if !ok {
		return 0, false
	}
	line := l.Lines[lineIdx]
	head := l.Text[line.ByteIdx:byteIdx]
	return line.Utf16Idx + lenUtf16(head), true
}

// ByteToLine returns the index of the line that contains the given byte index.
//
// It returns false if the byte index is out of bounds.
func (l *Lines) ByteToLine(byteIdx uint) (uint, bool) {
	if int(byteIdx) > len(l.Text) {
		return 0, false
	}
	i := sort.Search(len(l.Lines), func(i int) bool {
		return l.Lines[i].ByteIdx > byteIdx
	})
	return uint(i) - 1, true
}

// ByteToColumn returns the index of the column at the byte index.
//
// The column is defined as the number of characters in the line before the
// byte index. It returns false if the byte index is out of bounds.
func (l *Lines) ByteToColumn(byteIdx uint) (uint, bool) {
	line, ok := l.ByteToLine(byteIdx)
	if !ok {
		return 0, false
	}
	start, ok := l.LineToByte(line)
	if !ok {
		return 0, false
	}
	head := l.Text[start:byteIdx]
	col := uint(utf8.RuneCountInString(head))
	return col, true
}

// ByteToLineColumn returns the index of the line and column at the byte index.
//
// It returns false if the byte index is out of bounds.
func (l *Lines) ByteToLineColumn(byteIdx uint) (Position, bool) {
	line, ok := l.ByteToLine(byteIdx)
	if !ok {
		return Position{}, false
	}
	col, ok := l.ByteToColumn(byteIdx)
	if !ok {
		return Position{}, false
	}
	pos := Position{
		Line: line,
		Col:  col,
	}
	return pos, true
}

// Utf16ToByte returns the byte index at the UTF-16 code unit.
//
// It returns false if the UTF-16 index is out of bounds.
func (l *Lines) Utf16ToByte(utf16Idx uint) (uint, bool) {
	i := sort.Search(len(l.Lines), func(i int) bool {
		return l.Lines[i].Utf16Idx > utf16Idx
	})
	i--
	line := l.Lines[i]
	text := l.Text
	k := line.Utf16Idx
	j := 0
	for {
		if k >= utf16Idx {
			return line.ByteIdx + uint(j), true
		}
		r, n := utf8.DecodeRuneInString(text[line.ByteIdx+uint(j):])
		if n == 0 {
			break
		}
		j += n
		k += uint(utf16.RuneLen(r))
	}
	if k == utf16Idx {
		return uint(len(text)), true
	}
	return 0, false
}

// LineToByte returns the byte position at which the given line starts.
//
// It returns false if the line index is out of bounds.
func (l *Lines) LineToByte(lineIdx uint) (uint, bool) {
	if int(lineIdx) >= len(l.Lines) {
		return 0, false
	}
	return l.Lines[lineIdx].ByteIdx, true
}

// LineToRange returns the range which encloses the given line.
//
// It returns false if the line index is out of bounds.
func (l *Lines) LineToRange(lineIdx uint) (ranges.Range, bool) {
	start, ok := l.LineToByte(lineIdx)
	if !ok {
		return ranges.Range{}, false
	}
	end, ok := l.LineToByte(lineIdx + 1)
	if !ok {
		end = uint(len(l.Text))
	}
	return ranges.NewRange(uint64(start), uint64(end)), true
}

// LineColumnToByte returns the byte index of the given (line, column) pair.
//
// The column defines the number of characters to go beyond the start of
// the line. It returns false if the line or column is out of bounds.
func (l *Lines) LineColumnToByte(lineIdx uint, columnIdx uint) (uint, bool) {
	r, ok := l.LineToRange(lineIdx)
	if !ok {
		return 0, false
	}
	line := l.Text[r.Start:r.End]
	chars := []rune(line)
	if int(columnIdx) > len(chars) {
		return 0, false
	}
	head := string(chars[:columnIdx])
	return uint(r.Start) + uint(len(head)), true
}

// Replace fully replaces the source text.
//
// It performs a naive (suffix/prefix-based) diff of the old and new text to
// produce the smallest single edit that transforms the old source into the new one.
//
// It returns whether any changes were made.
func (l *Lines) Replace(new string) bool {
	replacementRange, ok := l.ReplacementRange(new)
	if !ok {
		return false
	}
	old := l.Text
	replace := ranges.NewRange(uint64(replacementRange.Prefix), uint64(uint(len(old))-replacementRange.Suffix))
	newRange := ranges.NewRange(uint64(replacementRange.Prefix), uint64(uint(len(new))-replacementRange.Suffix))
	with := new[newRange.Start:newRange.End]
	l.Edit(replace, with)
	return true
}

// ReplacementRange holds the common prefix and suffix lengths.
type ReplacementRange struct {
	Prefix uint
	Suffix uint
}

// ReplacementRange returns the common prefix and suffix lengths.
//
// It returns false if the old and new strings are equal.
func (l *Lines) ReplacementRange(new string) (ReplacementRange, bool) {
	old := l.Text

	if old == new {
		return ReplacementRange{}, false
	}

	n := min(len(old), len(new))
	prefix := 0
	for i := 0; i < n; i++ {
		if old[i] == new[i] {
			prefix++
		}
	}

	for !stdx.IsCharBoundary(old, uint(prefix)) || !stdx.IsCharBoundary(new, uint(prefix)) {
		prefix--
	}

	suffix := 0
	for i, j := len(old)-1, len(new)-1; i >= prefix && j >= prefix; i, j = i-1, j-1 {
		if old[i] == new[j] {
			suffix++
		}
	}

	for !stdx.IsCharBoundary(old, uint(len(old)-suffix)) || !stdx.IsCharBoundary(new, uint(len(new)-suffix)) {
		suffix++
	}

	return ReplacementRange{
		Prefix: uint(prefix),
		Suffix: uint(suffix),
	}, true
}

// Edit edits the source file by replacing the given range.
//
// The method panics if the replace range is out of bounds.
func (l *Lines) Edit(replace ranges.Range, with string) {
	startByte := uint(replace.Start)
	startUtf16, ok := l.ByteToUtf16(startByte)
	if !ok {
		panic("invalid byte index in replace range")
	}
	line, ok := l.ByteToLine(startByte)
	if !ok {
		panic("invalid byte index in replace range")
	}

	// Update the text itself.
	l.Text = replaceRange(l.Text, replace, with)

	// Remove invalidated line starts.
	l.Lines = l.Lines[:line+1]

	// Handle adjoining of \r and \n.
	if strings.HasSuffix(l.Text[:startByte], "\r") && strings.HasPrefix(with, "\n") {
		l.Lines = l.Lines[:len(l.Lines)-1]
	}

	// Recalculate the line starts after the edit.
	newLines := linesFrom(startByte, startUtf16, l.Text[startByte:])
	l.Lines = append(l.Lines, newLines...)
}

// Compute a line iterator from an offset.
func linesFrom(byteOffset, utf16Offset uint, text string) []Line {
	var lines []Line
	s := scanner.New(text)
	utf16Idx := utf16Offset
	for {
		stop := func(c rune) bool {
			utf16Idx += uint(utf16RuneLen(c))
			return isNewline(c)
		}
		s.EatUntilFunc(stop)

		if s.Done() {
			break
		}

		if c, ok := s.Eat(); ok && c == '\r' && s.EatIf("\n") {
			utf16Idx++
		}

		line := Line{
			ByteIdx:  byteOffset + uint(s.Cursor()),
			Utf16Idx: utf16Idx,
		}
		lines = append(lines, line)
	}
	return lines
}

// utf16RuneLen returns the length of the given UTF-16 rune in number of bytes.
func utf16RuneLen(c rune) int {
	n := utf16.RuneLen(c)
	if n == -1 {
		panic(fmt.Errorf("invalid utf16 character %[1]U %[1]q", c))
	}
	return n // number of 16-bit words used by character.
}

// lenUtf16 returns the number of code units this string would use if it was
// encoded in UTF16.
func lenUtf16(str string) uint {
	chars := []rune(str)
	total := uint(0)
	for _, c := range chars {
		n := utf16.RuneLen(c)
		if n == -1 {
			panic(fmt.Errorf("invalid utf16 character %[1]U %[1]q", c))
		}
		total += uint(n)
	}
	return total
}

// Position represents a line and column pair.
type Position struct {
	Line uint
	Col  uint
}

// replaceRange replaces the given range in str.
func replaceRange(str string, r ranges.Range, with string) string {
	pre := str[:r.Start]
	post := str[r.End:]
	return pre + with + post
}

package syntax

import (
	"fmt"
	"iter"
	"slices"
	"sort"
	"strings"
	"unicode/utf16"
	"unicode/utf8"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
	"github.com/mewmew/typast/internal/scanner"
)

// --- [ Lines ] ---------------------------------------------------------------

// A text buffer and metadata about lines.
//
// This is internally reference-counted and thus cheap to clone.
type Lines struct {
	lines []Line
	// The text as a string slice.
	text string
}

// Metadata about a line.
type Line struct {
	// The UTF-8 byte offset where the line starts.
	byte_idx uint
	// The UTF-16 codepoint offset where the line starts.
	utf16_idx uint
}

// Create from the text buffer and compute the line metadata.
//
// new
func NewLines(text string) *Lines {
	lines := lines_from_string(text)
	return &Lines{
		lines: lines,
		text:  text,
	}
}

// Get the length of the file in UTF-8 encoded bytes.
func (l *Lines) len_bytes() uint {
	return uint(len(l.text))
}

// Get the length of the file in UTF-16 code units.
func (l *Lines) len_utf16() uint {
	last := l.lines[len(l.lines)-1]
	return last.utf16_idx + len_utf16(l.text[last.byte_idx:])
}

// Get the length of the file in lines.
func (l *Lines) len_lines() uint {
	return uint(len(l.lines))
}

// Return the index of the UTF-16 code unit at the byte index.
func (l *Lines) byte_to_utf16(byte_idx uint) option.Option[uint] {
	line_idx, ok := l.byte_to_line(byte_idx).Get()
	if !ok {
		return option.None[uint]()
	}
	line := l.lines[line_idx]
	head := l.text[line.byte_idx:byte_idx]
	return option.Some(line.utf16_idx + len_utf16(head))
}

// Return the index of the line that contains the given byte index.
func (l *Lines) byte_to_line(byte_idx uint) option.Option[uint] {
	// TODO: double-check translation from Rust to Go (should be `>=` ?).
	if int(byte_idx) > len(l.text) {
		return option.None[uint]()
	}
	i := sort.Search(len(l.lines), func(i int) bool {
		return l.lines[i].byte_idx >= byte_idx
	})
	if !(i < len(l.lines) && l.lines[i].byte_idx == byte_idx) {
		i--
	}
	return option.Some(uint(i))
}

// Return the index of the column at the byte index.
//
// The column is defined as the number of characters in the line before the
// byte index.
func (l *Lines) byte_to_column(byte_idx uint) option.Option[uint] {
	line := l.byte_to_line(byte_idx).MustGet()
	start := l.line_to_byte(line).MustGet()
	head := l.text[start:byte_idx]
	col := uint(len(chars(head)))
	return option.Some(col)
}

// Return the index of the line and column at the byte index.
func (l *Lines) byte_to_line_column(byte_idx uint) option.Option[Position] {
	line := l.byte_to_line(byte_idx).MustGet()
	start := l.line_to_byte(line).MustGet()
	head := l.text[start:byte_idx]
	col := uint(len(chars(head)))
	pos := Position{
		line: line,
		col:  col,
	}
	return option.Some(pos)
}

// Return the byte index at the UTF-16 code unit.
func (l *Lines) utf16_to_byte(utf16_idx uint) option.Option[uint] {
	i := sort.Search(len(l.lines), func(i int) bool {
		return l.lines[i].utf16_idx >= utf16_idx
	})
	if !(i < len(l.lines) && l.lines[i].utf16_idx == utf16_idx) {
		i--
	}

	// TODO: double-check translation from Rust to Go.
	line := l.lines[i]
	text := l.text
	k := line.utf16_idx

	j := 0
	for len(text[line.byte_idx+uint(j):]) > 0 {
		if k >= utf16_idx {
			return option.Some(line.byte_idx + uint(j))
		}
		c, n := utf8.DecodeRuneInString(text[line.byte_idx+uint(j):])
		if n == 0 {
			panic("empty string")
		}
		j += n
		k += uint(char_len_utf16(c))
	}
	if k == utf16_idx {
		return option.Some(uint(len(text)))
	}
	return option.None[uint]()
}

// Return the byte position at which the given line starts.
func (l *Lines) line_to_byte(line_idx uint) option.Option[uint] {
	if int(line_idx) >= len(l.lines) {
		return option.None[uint]()
	}
	return option.Some(l.lines[line_idx].byte_idx)
}

// Return the range which encloses the given line.
func (l *Lines) line_to_range(line_idx uint) option.Option[ranges.Range] {
	start := l.line_to_byte(line_idx).MustGet()
	end, ok := l.line_to_byte(line_idx + 1).Get()
	if !ok {
		end = uint(len(l.text))
	}
	return option.Some(ranges.NewRange(uint64(start), uint64(end)))
}

// Return the byte index of the given (line, column) pair.
//
// The column defines the number of characters to go beyond the start of
// the line.
func (l *Lines) line_column_to_byte(line_idx uint, column_idx uint) option.Option[uint] {
	_range := l.line_to_range(line_idx).MustGet()
	line := l.text[_range.Start:_range.End]
	cs := chars(line)
	return option.Some(uint(_range.Start + uint64(len(line)-len(string(cs[column_idx:])))))
}

// Fully replace the source text.
//
// This performs a naive (suffix/prefix-based) diff of the old and new text
// to produce the smallest single edit that transforms old into new and
// then calls [`edit`](Self::edit) with it.
//
// Returns whether any changes were made.
func (l *Lines) replace(new string) bool {
	_replacement_range, ok := l.replacement_range(new).Get()
	if !ok {
		return false
	}
	old := l.text
	replace := ranges.NewRange(uint64(_replacement_range.prefix), uint64(uint(len(old))-_replacement_range.suffix))
	new_range := ranges.NewRange(uint64(_replacement_range.prefix), uint64(uint(len(new))-_replacement_range.suffix))
	with := new[new_range.Start:new_range.End]
	l.edit(replace, with)

	return true
}

type ReplacementRange struct {
	prefix uint
	suffix uint
}

// Returns the common prefix and suffix lengths.
// Returns [`None`] if the old and new strings are equal.
func (l *Lines) replacement_range(new string) option.Option[ReplacementRange] {
	old := l.text

	n := min(len(old), len(new))
	prefix := 0
	for i := 0; i < n; i++ {
		if old[i] == new[i] {
			prefix++
		}
	}

	if prefix == len(old) && prefix == len(new) {
		return option.None[ReplacementRange]()
	}

	for !is_char_boundary(old, prefix) || !is_char_boundary(new, prefix) {
		prefix -= 1
	}

	suffix := 0
	for i, j := len(old)-1, len(new)-1; i >= prefix && j >= prefix; i, j = i-1, j-1 {
		if old[i] == new[j] {
			suffix++
		}
	}

	for !is_char_boundary(old, len(old)-suffix) || !is_char_boundary(new, len(new)-suffix) {
		suffix += 1
	}

	_range := ReplacementRange{
		prefix: uint(prefix),
		suffix: uint(suffix),
	}
	return option.Some(_range)
}

// Edit the source file by replacing the given range.
//
// Returns the range in the new source that was ultimately reparsed.
//
// The method panics if the `replace` range is out of bounds.
func (l *Lines) edit(replace ranges.Range, with string) {
	start_byte := uint(replace.Start)
	start_utf16 := l.byte_to_utf16(start_byte).MustGet()
	line := l.byte_to_line(start_byte).MustGet()

	// Update the text itself.
	l.text = string_replace_range(l.text, replace, with)

	// Remove invalidated line starts.
	l.lines = l.lines[:line+1] // l.lines.truncate(line+1)

	// Handle adjoining of \r and \n.
	if strings.HasSuffix(l.text[:start_byte], "\r") && strings.HasPrefix(with, "\n") {
		l.lines = l.lines[:len(l.lines)-1] // l.lines.pop()
	}

	// Recalculate the line starts after the edit.
	new_lines := slices.Collect(lines_from(start_byte, start_utf16, l.text[start_byte:]))
	l.lines = append(l.lines, new_lines...)
}

// --- [/ Lines ] --------------------------------------------------------------

// Create a line vector.
//
// lines
func lines_from_string(text string) []Line {
	var lines []Line
	line := Line{
		byte_idx:  0,
		utf16_idx: 0,
	}
	lines = append(lines, line)
	lines = append(lines, slices.Collect(lines_from(0, 0, text))...)
	return lines
}

// Compute a line iterator from an offset.
func lines_from(byte_offset uint, utf16_offset uint, text string) iter.Seq[Line] {
	s := scanner.NewScanner(text)
	utf16_idx := utf16_offset
	return func(yield func(Line) bool) {
		// TODO: double-check that conversion from Rust to Go was correct.
		for {
			stop := func(c rune) bool {
				utf16_idx += uint(char_len_utf16(c))
				return is_newline(c)
			}
			s.EatUntilFunc(stop)

			if s.Done() {
				return
			}

			if c, ok := s.Eat(); ok && c == '\r' && s.EatIf('\n') {
				utf16_idx += 1
			}

			line := Line{
				byte_idx:  byte_offset + uint(s.Cursor()),
				utf16_idx: utf16_idx,
			}
			if !yield(line) {
				return
			}
		}
	}
}

// The number of code units this string would use if it was encoded in
// UTF16. This runs in linear time.
func len_utf16(str string) uint {
	total := 0
	for _, c := range chars(str) {
		n := utf16.RuneLen(c)
		if n == -1 {
			panic(fmt.Errorf("invalid utf16 character %[1]U %[1]q", c))
		}
		total += n
	}
	return uint(total)
}

func chars(str string) []rune {
	return []rune(str)
}

type Position struct {
	line uint
	col  uint
}

// len_utf16
func char_len_utf16(c rune) int {
	n := utf16.RuneLen(c)
	if n == -1 {
		panic(fmt.Errorf("invalid utf16 character %[1]U %[1]q", c))
	}
	return n // number of 16-bit words used by character.
}

func is_char_boundary(str string, index int) bool {
	// The start and end of the string (when index == len(str)) are considered to
	// be boundaries.
	if index == 0 || index == len(str) {
		return true
	}
	c, _ := utf8.DecodeRuneInString(str[index:])
	return c != utf8.RuneError
}

func string_replace_range(str string, _range ranges.Range, with string) string {
	pre := str[:_range.Start]
	post := str[_range.End:]
	return pre + with + post
}

func is_newline(c rune) bool {
	switch c {
	case '\r', '\n':
		return true
	}
	return false
}

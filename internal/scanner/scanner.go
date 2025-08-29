// NOTE: based on unscanny API: https://docs.rs/unscanny/latest/unscanny/struct.Scanner.html

package scanner

import (
	"fmt"
	"slices"
	"strings"
	"unicode/utf8"

	"github.com/mewmew/typast/internal/ranges"
	"github.com/mewmew/typast/internal/stdx"
)

// Scaner is a Unicode-aware string scanner.
type Scanner struct {
	// The string to scan.
	text string
	// The current index in the string.
	cursor uint
}

// New returns a new scanner for the given text, starting with a cursor position
// of 0.
func New(text string) *Scanner {
	return &Scanner{
		text:   text,
		cursor: 0,
	}
}

// Text returns the full input source text of the scanner.
func (s *Scanner) Text() string {
	return s.text
}

// Cursor returns the current cursor position.
func (s *Scanner) Cursor() uint {
	return s.cursor
}

// Done reports whether the scanner has fully consumed the string.
func (s *Scanner) Done() bool {
	return s.cursor == uint(len(s.text))
}

// Before returns the substring before the cursor.
func (s *Scanner) Before() string {
	s.checkCharBoundary(s.cursor) // sanity check
	return s.text[:s.cursor]
}

// After returns the substring after the cursor.
func (s *Scanner) After() string {
	s.checkCharBoundary(s.cursor) // sanity check
	return s.text[s.cursor:]
}

// From returns the substring from start to the cursor.
//
// Snaps start into the bounds of the string and to the next character
// boundary.
func (s *Scanner) From(start uint) string {
	start = s.snap(start)         // sanity check
	start = min(start, s.cursor)  // sanity check
	s.checkCharBoundary(start)    // sanity check
	s.checkCharBoundary(s.cursor) // sanity check
	return s.text[start:s.cursor]
}

// To returns the substring from the cursor to end.
//
// Snaps end into the bounds of the string and to the next character boundary.
func (s *Scanner) To(end uint) string {
	end = s.snap(end)             // sanity check
	end = max(s.cursor, end)      // sanity check
	s.checkCharBoundary(s.cursor) // sanity check
	s.checkCharBoundary(end)      // sanity check
	return s.text[s.cursor:end]
}

// Get returns the substring from start to end.
//
// Snaps start and end into the bounds of the string and to the next character
// boundary.
func (s *Scanner) Get(r ranges.Range) string {
	start := s.snap(uint(r.Start))         // sanity check
	end := max(start, s.snap(uint(r.End))) // sanity check
	s.checkCharBoundary(start)             // sanity check
	s.checkCharBoundary(end)               // sanity check
	return s.text[start:end]
}

// Peek peeks at the next character.
func (s *Scanner) Peek() (rune, bool) {
	for _, c := range s.After() {
		return c, true
	}
	return 0, false
}

// At reports whether the text after the cursor starts with the given pattern.
func (s *Scanner) At(pattern string) bool {
	after := s.After()
	return strings.HasPrefix(after, pattern)
}

// AtFunc reports whether the text after the cursor starts with the given
// pattern (a character that satisfies cond).
func (s *Scanner) AtFunc(cond func(rune) bool) bool {
	after := s.After()
	for _, c := range after {
		if cond(c) {
			return true
		}
		break
	}
	return false
}

// AtAny reports whether the text after the cursor starts with the given
// pattern (any of the given characters).
func (s *Scanner) AtAny(valid []rune) bool {
	cond := func(c rune) bool {
		return slices.Contains(valid, c)
	}
	return s.AtFunc(cond)
}

// Scout look at the n-th character relative to the cursor without changing the
// cursor.
//
//   - `scout(-1)` is the character before the cursor.
//   - `scout(0)` is the same as `peek()`.
func (s *Scanner) Scout(n int) (rune, bool) {
	if n >= 0 {
		after := s.After()
		i := 0
		for _, c := range after {
			if i == n {
				return c, true
			}
			i++
		}
	} else {
		before := s.Before()
		rs := []rune(before) // TODO: optimize when needed.
		index := len(rs) + n
		if index >= 0 && index < len(rs) {
			return rs[index], true
		}
	}
	return 0, false
}

// TODO: implement Locate.

// Eat consumes and return the character right after the cursor.
func (s *Scanner) Eat() (rune, bool) {
	c, ok := s.Peek()
	if !ok {
		return 0, false
	}
	s.cursor += uint(utf8.RuneLen(c))
	return c, true
}

// Uneat unconsumes and return the character right before the cursor, moving it
// back.
func (s *Scanner) Uneat() (rune, bool) {
	before := s.Before()
	rs := []rune(before) // TODO: optimize when needed.
	if len(rs) == 0 {
		return 0, false
	}
	c := rs[len(rs)-1]
	s.cursor -= uint(utf8.RuneLen(c))
	return c, true
}

// EatIf consumes the given pattern if that's what's right after the cursor.
//
// Returns true if the pattern was consumed.
func (s *Scanner) EatIf(pattern string) bool {
	after := s.After()
	if !strings.HasPrefix(after, pattern) {
		return false
	}
	s.cursor += uint(len(pattern))
	return true
}

// EatIfFunc consumes the given pattern (a sequence of characters that satisfy
// cond) if that's what's right after the cursor.
//
// Returns true if the pattern was consumed.
func (s *Scanner) EatIfFunc(cond func(rune) bool) bool {
	after := s.After()
	n := 0
	for _, c := range after {
		if !cond(c) {
			break
		}
		n += utf8.RuneLen(c)
	}
	if n == 0 {
		return false
	}
	s.cursor += uint(n)
	return true
}

// EatIfAny consumes the given pattern (a sequence of any of the given
// characters) if that's what's right after the cursor.
//
// Returns true if the pattern was consumed.
func (s *Scanner) EatIfAny(valid []rune) bool {
	cond := func(c rune) bool {
		return slices.Contains(valid, c)
	}
	return s.EatIfFunc(cond)
}

// EatWhileFunc consumes while the given pattern (a sequence of characters that
// satisfy cond) is what's right after the cursor.
//
// Returns the consumed substring.
func (s *Scanner) EatWhileFunc(cond func(c rune) bool) string {
	start := s.cursor
	after := s.After()
	n := 0
	for _, c := range after {
		if !cond(c) {
			break
		}
		n += utf8.RuneLen(c)
	}
	s.cursor += uint(n)
	return s.From(start)
}

// EatUntilFunc consumes until the given pattern (a character that satisfy cond)
// is what's right after the cursor.
//
// Returns the consumed substring.
func (s *Scanner) EatUntilFunc(cond func(c rune) bool) string {
	_cond := func(c rune) bool {
		return !cond(c)
	}
	return s.EatWhileFunc(_cond)
}

// Expect consumes the given pattern if that's what's right after the cursor or
// panic otherwise.
func (s *Scanner) Expect(pattern string) {
	if !s.EatIf(pattern) {
		panic(fmt.Sprintf("expected %q prefix, got %q", pattern, s.After()))
	}
}

// Jump jumps to a byte position in the source string.
//
// Snaps into the bounds of the string and to the next character boundary.
func (s *Scanner) Jump(target uint) {
	s.cursor = s.snap(target)
}

// ### [ Utility functions ] ###################################################

// snap snaps an index in-bounds and to a valid character boundary.
func (s *Scanner) snap(index uint) uint {
	index = min(index, uint(len(s.text)))
	for !stdx.IsCharBoundary(s.text, index) {
		index--
	}
	return index
}

// checkCharBoundary performs a sanity check to ensure that the given index is
// at a character boundary.
func (s *Scanner) checkCharBoundary(index uint) {
	if !stdx.IsCharBoundary(s.text, index) {
		panic("not on character boundary")
	}
}

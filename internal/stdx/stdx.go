// NOTE: based on char API: https://doc.rust-lang.org/std/primitive.char.html
// NOTE: based on str API: https://doc.rust-lang.org/std/primitive.str.html

package stdx

import (
	"unicode"
	"unicode/utf8"
)

// IsCharBoundary checks that index-th byte is the first byte in a UTF-8 code
// point sequence or the end of the string.
//
// is_char_boundary
func IsCharBoundary(str string, index uint) bool {
	// The start and end of the string (when index == len(str)) are considered to
	// be boundaries.
	if index == 0 || index == uint(len(str)) {
		return true
	}
	c, _ := utf8.DecodeRuneInString(str[index:])
	return c != utf8.RuneError
}

// IsWhitespace reports whether the given character has the White_Space
// property.
//
// is_whitespace
func IsWhitespace(c rune) bool {
	return unicode.IsSpace(c)
}

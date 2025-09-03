// NOTE: based on char API: https://doc.rust-lang.org/std/primitive.char.html
// NOTE: based on str API: https://doc.rust-lang.org/std/primitive.str.html

package stdx

import (
	"strings"
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

// is_ascii_lowercase
func IsAsciiLowercase(c rune) bool {
	const alphabet = "abcdefghijklmnopqrstuvwxyz"
	return strings.ContainsRune(alphabet, c)
}

// is_ascii_uppercase
func IsAsciiUppercase(c rune) bool {
	const ALPHABET = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
	return strings.ContainsRune(ALPHABET, c)
}

// is_ascii_alphabetic
func IsAsciiAlphabetic(c rune) bool {
	return IsAsciiLowercase(c) || IsAsciiUppercase(c)
}

// is_ascii_digit
func IsAsciiDigit(c rune) bool {
	const digits = "0123456789"
	return strings.ContainsRune(digits, c)
}

// is_ascii_alphanumeric
func IsAsciiAlphanumeric(c rune) bool {
	return IsAsciiAlphabetic(c) || IsAsciiDigit(c)
}

// is_ascii_hexdigit
func IsAsciiHexdigit(c rune) bool {
	const hex = "abcdefABCDEF"
	return IsAsciiDigit(c) || strings.ContainsRune(hex, c)
}

// is_alphabetic
func IsAlphabetic(c rune) bool {
	return unicode.IsLetter(c)
}

// is_numeric
func IsNumeric(c rune) bool {
	return unicode.IsNumber(c)
}

// is_alphanumeric
func IsAlphanumeric(c rune) bool {
	return IsAlphabetic(c) || IsNumeric(c)
}

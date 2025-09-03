// NOTE: based on char API: https://doc.rust-lang.org/std/primitive.char.html
// NOTE: based on str API: https://doc.rust-lang.org/std/primitive.str.html

package stdx

import (
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/mewmew/mathclass"
	"github.com/mewmew/typast/internal/option"
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

type MathClass uint8

const (
	MathClass_Normal MathClass = iota + 1
	MathClass_Alphabetic
	MathClass_Binary
	MathClass_Closing
	MathClass_Diacritic
	MathClass_Fence
	MathClass_Glyph_Part
	MathClass_Large
	MathClass_Opening
	MathClass_Punctuation
	MathClass_Relation
	MathClass_Space
	MathClass_Unary
	MathClass_Vary
	MathClass_Special
)

func MathClassFromChar(c rune) option.Option[MathClass] {
	var classes = map[MathClass]*unicode.RangeTable{
		MathClass_Normal:      mathclass.Normal,
		MathClass_Alphabetic:  mathclass.Alphabetic,
		MathClass_Binary:      mathclass.Binary,
		MathClass_Closing:     mathclass.Closing,
		MathClass_Diacritic:   mathclass.Diacritic,
		MathClass_Fence:       mathclass.Fence,
		MathClass_Glyph_Part:  mathclass.Glyph_Part,
		MathClass_Large:       mathclass.Large,
		MathClass_Opening:     mathclass.Opening,
		MathClass_Punctuation: mathclass.Punctuation,
		MathClass_Relation:    mathclass.Relation,
		MathClass_Space:       mathclass.Space,
		MathClass_Unary:       mathclass.Unary,
		MathClass_Vary:        mathclass.Vary,
		MathClass_Special:     mathclass.Special,
	}
	for class, table := range classes {
		if unicode.In(c, table) {
			return option.Some(class)
		}
	}
	return option.None[MathClass]()
}

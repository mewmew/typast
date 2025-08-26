package syntax

import (
	"unicode"
)

// Whether a string is a valid Typst identifier.
//
// In addition to what is specified in the [Unicode Standard][uax31], we allow:
// - `_` as a starting character,
// - `_` and `-` as continuing characters.
//
// [uax31]: http://www.unicode.org/reports/tr31/
func is_ident(str string) bool {
	rs := []rune(str)
	if len(rs) == 0 {
		return false
	}
	for i, r := range rs {
		if i == 0 {
			if !is_id_start(r) {
				return false
			}
		} else {
			if !is_id_continue(r) {
				return false
			}
		}
	}
	return true
}

// / Whether a character can start an identifier.
func is_id_start(c rune) bool {
	return is_xid_start(c) || c == '_'
}

// / Whether a character can continue an identifier.
func is_id_continue(c rune) bool {
	return is_xid_continue(c) || c == '_' || c == '-'
}

func is_xid_start(r rune) bool {
	return unicode.In(r, unicode.Other_ID_Start)
}

func is_xid_continue(r rune) bool {
	return unicode.In(r, unicode.Other_ID_Continue)
}

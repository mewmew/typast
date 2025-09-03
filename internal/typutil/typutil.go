package typutil

import (
	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/stdx"
)

// Returns the default math class of a character in Typst, if it has one.
//
// This is determined by the Unicode math class, with some manual overrides.
func DefaultMathClass(c rune) option.Option[stdx.MathClass] {
	switch c {
	// Better spacing.
	// https://github.com/typst/typst/commit/2e039cb052fcb768027053cbf02ce396f6d7a6be
	case ':':
		return option.Some(stdx.MathClass_Relation)

	// Better spacing when used alongside + PLUS SIGN.
	// https://github.com/typst/typst/pull/1726
	case '⋯', '⋱', '⋰', '⋮':
		return option.Some(stdx.MathClass_Normal)

	// Better spacing.
	// https://github.com/typst/typst/pull/1855
	case '.', '/':
		return option.Some(stdx.MathClass_Normal)

	// ⊥ UP TACK should not be a relation, contrary to ⟂ PERPENDICULAR.
	// https://github.com/typst/typst/pull/5714
	case '\u22A5':
		return option.Some(stdx.MathClass_Normal)

	// Used as a binary connector in linear logic, where it is referred to
	// as "par".
	// https://github.com/typst/typst/issues/5764
	case '⅋':
		return option.Some(stdx.MathClass_Binary)

	// Those overrides should become the default in the next revision of
	// MathClass.txt.
	// https://github.com/typst/typst/issues/5764#issuecomment-2632435247
	case '⎰', '⟅':
		return option.Some(stdx.MathClass_Opening)
	case '⎱', '⟆':
		return option.Some(stdx.MathClass_Closing)

	// Both ∨ and ⟑ are classified as Binary.
	// https://github.com/typst/typst/issues/5764
	case '⟇':
		return option.Some(stdx.MathClass_Binary)

	// Arabic comma.
	// https://github.com/latex3/unicode-math/pull/633#issuecomment-2028936135
	case '،':
		return option.Some(stdx.MathClass_Punctuation)

	default:
		return stdx.MathClassFromChar(c)
	}
}

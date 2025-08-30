// Acknowledgement:
// Based on rust-analyzer's `TokenSet`.
// https://github.com/rust-lang/rust-analyzer/blob/master/crates/parser/src/token_set.rs

package syntax

import (
	"github.com/bits-and-blooms/bitset"
)

// --- [ SyntaxSet ] -----------------------------------------------------------

// A set of syntax kinds.
type SyntaxSet struct {
	set *bitset.BitSet
}

// Create a new set from a slice of kinds.
//
// new
func NewSyntaxSet(kinds ...SyntaxKind) *SyntaxSet {
	set := &SyntaxSet{
		set: bitset.New(NSyntaxKinds),
	}
	for _, kind := range kinds {
		set.add(kind)
	}
	return set
}

// Insert a syntax kind into the set.
//
// You can only add kinds with discriminator < 128.
func (set *SyntaxSet) add(kind SyntaxKind) *SyntaxSet {
	set.set.Set(bit_num(kind))
	return set
}

// Combine two syntax sets.
func (set *SyntaxSet) union(other *SyntaxSet) *SyntaxSet {
	return &SyntaxSet{
		set: set.set.Union(other.set),
	}
}

// Whether the set contains the given syntax kind.
func (set *SyntaxSet) contains(kind SyntaxKind) bool {
	return set.set.Test(bit_num(kind))
}

func bit_num(kind SyntaxKind) uint {
	return uint(kind) - 1 // convert 1-index kind into 0-indexed bit.
}

// Syntax kinds that can start a statement.
var STMT = NewSyntaxSet(
	SyntaxKindLet,
	SyntaxKindSet,
	SyntaxKindShow,
	SyntaxKindImport,
	SyntaxKindInclude,
	SyntaxKindReturn,
)

// Syntax kinds that can start a math expression.
var MATH_EXPR = NewSyntaxSet(
	SyntaxKindHash,
	SyntaxKindMathIdent,
	SyntaxKindFieldAccess,
	SyntaxKindDot,
	SyntaxKindComma,
	SyntaxKindSemicolon,
	SyntaxKindRightParen,
	SyntaxKindText,
	SyntaxKindMathText,
	SyntaxKindMathShorthand,
	SyntaxKindLinebreak,
	SyntaxKindMathAlignPoint,
	SyntaxKindEscape,
	SyntaxKindStr,
	SyntaxKindRoot,
	SyntaxKindPrime,
)

// Syntax kinds that can start a code expression.
var CODE_EXPR = CODE_PRIMARY.union(UNARY_OP)

// Syntax kinds that can start an atomic code expression.
var ATOMIC_CODE_EXPR = ATOMIC_CODE_PRIMARY

// Syntax kinds that can start a code primary.
var CODE_PRIMARY = ATOMIC_CODE_PRIMARY.add(SyntaxKindUnderscore)

// Syntax kinds that can start an atomic code primary.
var ATOMIC_CODE_PRIMARY = NewSyntaxSet(
	SyntaxKindIdent,
	SyntaxKindLeftBrace,
	SyntaxKindLeftBracket,
	SyntaxKindLeftParen,
	SyntaxKindDollar,
	SyntaxKindLet,
	SyntaxKindSet,
	SyntaxKindShow,
	SyntaxKindContext,
	SyntaxKindIf,
	SyntaxKindWhile,
	SyntaxKindFor,
	SyntaxKindImport,
	SyntaxKindInclude,
	SyntaxKindBreak,
	SyntaxKindContinue,
	SyntaxKindReturn,
	SyntaxKindNone,
	SyntaxKindAuto,
	SyntaxKindInt,
	SyntaxKindFloat,
	SyntaxKindBool,
	SyntaxKindNumeric,
	SyntaxKindStr,
	SyntaxKindLabel,
	SyntaxKindRaw,
)

// Syntax kinds that are unary operators.
var UNARY_OP = NewSyntaxSet(
	SyntaxKindPlus,
	SyntaxKindMinus,
	SyntaxKindNot,
)

// Syntax kinds that are binary operators.
var BINARY_OP = NewSyntaxSet(
	SyntaxKindPlus,
	SyntaxKindMinus,
	SyntaxKindStar,
	SyntaxKindSlash,
	SyntaxKindAnd,
	SyntaxKindOr,
	SyntaxKindEqEq,
	SyntaxKindExclEq,
	SyntaxKindLt,
	SyntaxKindLtEq,
	SyntaxKindGt,
	SyntaxKindGtEq,
	SyntaxKindEq,
	SyntaxKindIn,
	SyntaxKindPlusEq,
	SyntaxKindHyphEq,
	SyntaxKindStarEq,
	SyntaxKindSlashEq,
)

// Syntax kinds that can start an argument in a function call.
var ARRAY_OR_DICT_ITEM = CODE_EXPR.add(SyntaxKindDots)

// Syntax kinds that can start an argument in a function call.
var ARG = CODE_EXPR.add(SyntaxKindDots)

// Syntax kinds that can start a parameter in a parameter list.
var PARAM = PATTERN.add(SyntaxKindDots)

// Syntax kinds that can start a destructuring item.
var DESTRUCTURING_ITEM = PATTERN.add(SyntaxKindDots)

// Syntax kinds that can start a pattern.
var PATTERN = PATTERN_LEAF.add(SyntaxKindLeftParen).add(SyntaxKindUnderscore)

// Syntax kinds that can start a pattern leaf.
var PATTERN_LEAF = ATOMIC_CODE_EXPR

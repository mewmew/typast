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
	b *bitset.BitSet
}

// Create a new set from a slice of kinds.
//
// new
func NewSyntaxSet(kinds ...SyntaxKind) *SyntaxSet {
	set := &SyntaxSet{
		b: bitset.New(NSyntaxKinds),
	}
	for _, kind := range kinds {
		set.b.Set(bit_num(kind))
	}
	return set
}

// Insert a syntax kind into the set.
//
// You can only add kinds with discriminator < 128.
func (set *SyntaxSet) add(kind SyntaxKind) *SyntaxSet {
	new := set.Clone() // don't modify original
	new.b.Set(bit_num(kind))
	return new
}

// Combine two syntax sets.
func (set *SyntaxSet) union(other *SyntaxSet) *SyntaxSet {
	new := set.Clone() // don't modify original
	new.b.Union(other.b)
	return new
}

// Whether the set contains the given syntax kind.
func (set *SyntaxSet) contains(kind SyntaxKind) bool {
	return set.b.Test(bit_num(kind))
}

func (set *SyntaxSet) Clone() *SyntaxSet {
	new := &SyntaxSet{
		b: set.b.Clone(),
	}
	return new
}

func bit_num(kind SyntaxKind) uint {
	return uint(kind) - 1 // convert 1-index kind into 0-indexed bit.
}

// Syntax kinds that can start a statement.
//
// STMT
var Set_STMT = NewSyntaxSet(
	SyntaxKindLet,
	SyntaxKindSet,
	SyntaxKindShow,
	SyntaxKindImport,
	SyntaxKindInclude,
	SyntaxKindReturn,
)

// Syntax kinds that can start a math expression.
//
// MATH_EXPR
var Set_MATH_EXPR = NewSyntaxSet(
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
//
// CODE_EXPR
var Set_CODE_EXPR = Set_CODE_PRIMARY.union(Set_UNARY_OP)

// Syntax kinds that can start an atomic code expression.
//
// ATOMIC_CODE_EXPR
var Set_ATOMIC_CODE_EXPR = Set_ATOMIC_CODE_PRIMARY

// Syntax kinds that can start a code primary.
//
// CODE_PRIMARY
var Set_CODE_PRIMARY = Set_ATOMIC_CODE_PRIMARY.add(SyntaxKindUnderscore)

// Syntax kinds that can start an atomic code primary.
//
// ATOMIC_CODE_PRIMARY
var Set_ATOMIC_CODE_PRIMARY = NewSyntaxSet(
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
//
// UNARY_OP
var Set_UNARY_OP = NewSyntaxSet(
	SyntaxKindPlus,
	SyntaxKindMinus,
	SyntaxKindNot,
)

// Syntax kinds that are binary operators.
//
// BINARY_OP
var Set_BINARY_OP = NewSyntaxSet(
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
//
// ARRAY_OR_DICT_ITEM
var Set_ARRAY_OR_DICT_ITEM = Set_CODE_EXPR.add(SyntaxKindDots)

// Syntax kinds that can start an argument in a function call.
//
// ARG
var Set_ARG = Set_CODE_EXPR.add(SyntaxKindDots)

// Syntax kinds that can start a parameter in a parameter list.
//
// PARAM
var Set_PARAM = Set_PATTERN.add(SyntaxKindDots)

// Syntax kinds that can start a destructuring item.
//
// DESTRUCTURING_ITEM
var Set_DESTRUCTURING_ITEM = Set_PATTERN.add(SyntaxKindDots)

// Syntax kinds that can start a pattern.
//
// PATTERN
var Set_PATTERN = Set_PATTERN_LEAF.add(SyntaxKindLeftParen).add(SyntaxKindUnderscore)

// Syntax kinds that can start a pattern leaf.
//
// PATTERN_LEAF
var Set_PATTERN_LEAF = Set_ATOMIC_CODE_EXPR

// Acknowledgement:
// Based on rust-analyzer's `TokenSet`.
// https://github.com/rust-lang/rust-analyzer/blob/master/crates/parser/src/token_set.rs

package syntax

import (
	"github.com/bits-and-blooms/bitset"
)

// SyntaxSet represents a set of syntax kinds.
type SyntaxSet struct {
	b *bitset.BitSet
}

// NewSyntaxSet creates a new set from a slice of kinds.
func NewSyntaxSet(kinds ...SyntaxKind) *SyntaxSet {
	b := bitset.New(NSyntaxKinds)
	for _, kind := range kinds {
		b.Set(bitNum(kind))
	}
	return &SyntaxSet{b: b}
}

// Add returns a new set with the given kind inserted.
// The original set is not modified.
func (set *SyntaxSet) Add(kind SyntaxKind) *SyntaxSet {
	clone := set.Clone()
	clone.b.Set(bitNum(kind))
	return clone
}

// Union returns a new set combining this set with another.
// The original set is not modified.
func (set *SyntaxSet) Union(other *SyntaxSet) *SyntaxSet {
	clone := set.Clone()
	clone.b.Union(other.b)
	return clone
}

// Contains reports whether the set contains the given syntax kind.
func (set *SyntaxSet) Contains(kind SyntaxKind) bool {
	return set.b.Test(bitNum(kind))
}

// Clone returns a copy of the set.
func (set *SyntaxSet) Clone() *SyntaxSet {
	return &SyntaxSet{b: set.b.Clone()}
}

func bitNum(kind SyntaxKind) uint {
	// convert 1-indexed kind into 0-indexed bit.
	return uint(kind) - 1
}

// StmtSet contains syntax kinds that can start a statement.
var StmtSet = NewSyntaxSet(
	SyntaxKindLet,
	SyntaxKindSet,
	SyntaxKindShow,
	SyntaxKindImport,
	SyntaxKindInclude,
	SyntaxKindReturn,
)

// MathExprSet contains syntax kinds that can start a math expression.
var MathExprSet = NewSyntaxSet(
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

// CodeExprSet contains syntax kinds that can start a code expression.
var CodeExprSet = CodePrimarySet.Union(UnaryOpSet)

// AtomicCodeExprSet contains syntax kinds that can start an atomic code expression.
var AtomicCodeExprSet = AtomicCodePrimarySet

// CodePrimarySet contains syntax kinds that can start a code primary.
var CodePrimarySet = AtomicCodePrimarySet.Add(SyntaxKindUnderscore)

// AtomicCodePrimarySet contains syntax kinds that can start an atomic code primary.
var AtomicCodePrimarySet = NewSyntaxSet(
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

// UnaryOpSet contains syntax kinds that are unary operators.
var UnaryOpSet = NewSyntaxSet(
	SyntaxKindPlus,
	SyntaxKindMinus,
	SyntaxKindNot,
)

// BinaryOpSet contains syntax kinds that are binary operators.
var BinaryOpSet = NewSyntaxSet(
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

// ArrayOrDictItemSet contains syntax kinds that can start an argument in a function call.
var ArrayOrDictItemSet = CodeExprSet.Add(SyntaxKindDots)

// ArgSet contains syntax kinds that can start an argument in a function call.
var ArgSet = CodeExprSet.Add(SyntaxKindDots)

// ParamSet contains syntax kinds that can start a parameter in a parameter list.
var ParamSet = PatternSet.Add(SyntaxKindDots)

// DestructuringItemSet contains syntax kinds that can start a destructuring item.
var DestructuringItemSet = PatternSet.Add(SyntaxKindDots)

// PatternSet contains syntax kinds that can start a pattern.
var PatternSet = PatternLeafSet.Add(SyntaxKindLeftParen).Add(SyntaxKindUnderscore)

// PatternLeafSet contains syntax kinds that can start a pattern leaf.
var PatternLeafSet = AtomicCodeExprSet

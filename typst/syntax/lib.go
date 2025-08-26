// Parser and syntax tree for Typst.

package syntax

// The syntax mode of a portion of Typst code.
type SyntaxMode uint8

const (
	// Text and markup, as in the top level.
	ModeMarkup SyntaxMode = iota + 1
	// Math atoms, operators, etc., as in equations.
	ModeMath
	// Keywords, literals and operators, as after hashes.
	ModeCode
)

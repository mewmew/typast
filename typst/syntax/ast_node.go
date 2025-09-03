package syntax

import "github.com/mewmew/typast/internal/option"

// Implements [`AstNode`] for a struct whose name matches a [`SyntaxKind`]
// variant.
//
// The struct becomes a wrapper around a [`SyntaxNode`] pointer, and the
// implementation of [`AstNode::from_untyped`] checks that the pointer's kind
// matches when converting, returning `Some` or `None` respectively.
//
// The generated struct is the basis for typed accessor methods for properties
// of this AST node. For example, the [`Raw`] struct has methods for accessing
// its content by lines, its optional language tag, and whether the raw element
// is inline or a block. These methods are accessible only _after_ a
// `SyntaxNode` is coerced to the `Raw` struct type (via `from_untyped`),
// guaranteeing their implementations will work with the expected structure.

// --- [ Markup ] --------------------------------------------------------------

// from_untyped
func (Markup) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMarkup {
		return option.Some[AstNode](&Markup{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Markup) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Markup_default() *Markup {
	node := SyntaxNode_placeholder(SyntaxKindMarkup)
	return &Markup{SyntaxNode: node}
}

// --- [ Text ] ----------------------------------------------------------------

// from_untyped
func (Text) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindText {
		return option.Some[AstNode](&Text{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Text) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Text_default() *Text {
	node := SyntaxNode_placeholder(SyntaxKindText)
	return &Text{SyntaxNode: node}
}

// --- [ Space ] ---------------------------------------------------------------

// from_untyped
func (Space) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSpace {
		return option.Some[AstNode](&Space{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Space) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Space_default() *Space {
	node := SyntaxNode_placeholder(SyntaxKindSpace)
	return &Space{SyntaxNode: node}
}

// --- [ Linebreak ] -----------------------------------------------------------

// from_untyped
func (Linebreak) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindLinebreak {
		return option.Some[AstNode](&Linebreak{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Linebreak) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Linebreak_default() *Linebreak {
	node := SyntaxNode_placeholder(SyntaxKindLinebreak)
	return &Linebreak{SyntaxNode: node}
}

// --- [ Parbreak ] ------------------------------------------------------------

// from_untyped
func (Parbreak) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindParbreak {
		return option.Some[AstNode](&Parbreak{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Parbreak) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Parbreak_default() *Parbreak {
	node := SyntaxNode_placeholder(SyntaxKindParbreak)
	return &Parbreak{SyntaxNode: node}
}

// --- [ Escape ] --------------------------------------------------------------

// from_untyped
func (Escape) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindEscape {
		return option.Some[AstNode](&Escape{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Escape) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Escape_default() *Escape {
	node := SyntaxNode_placeholder(SyntaxKindEscape)
	return &Escape{SyntaxNode: node}
}

// --- [ Shorthand ] -----------------------------------------------------------

// from_untyped
func (Shorthand) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindShorthand {
		return option.Some[AstNode](&Shorthand{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Shorthand) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Shorthand_default() *Shorthand {
	node := SyntaxNode_placeholder(SyntaxKindShorthand)
	return &Shorthand{SyntaxNode: node}
}

// --- [ SmartQuote ] ----------------------------------------------------------

// from_untyped
func (SmartQuote) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSmartQuote {
		return option.Some[AstNode](&SmartQuote{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *SmartQuote) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func SmartQuote_default() *SmartQuote {
	node := SyntaxNode_placeholder(SyntaxKindSmartQuote)
	return &SmartQuote{SyntaxNode: node}
}

// --- [ Strong ] --------------------------------------------------------------

// from_untyped
func (Strong) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindStrong {
		return option.Some[AstNode](&Strong{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Strong) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Strong_default() *Strong {
	node := SyntaxNode_placeholder(SyntaxKindStrong)
	return &Strong{SyntaxNode: node}
}

// --- [ Emph ] ----------------------------------------------------------------

// from_untyped
func (Emph) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindEmph {
		return option.Some[AstNode](&Emph{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Emph) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Emph_default() *Emph {
	node := SyntaxNode_placeholder(SyntaxKindEmph)
	return &Emph{SyntaxNode: node}
}

// --- [ Raw ] -----------------------------------------------------------------

// from_untyped
func (Raw) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindRaw {
		return option.Some[AstNode](&Raw{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Raw) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Raw_default() *Raw {
	node := SyntaxNode_placeholder(SyntaxKindRaw)
	return &Raw{SyntaxNode: node}
}

// --- [ RawLang ] -------------------------------------------------------------

// from_untyped
func (RawLang) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindRawLang {
		return option.Some[AstNode](&RawLang{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *RawLang) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func RawLang_default() *RawLang {
	node := SyntaxNode_placeholder(SyntaxKindRawLang)
	return &RawLang{SyntaxNode: node}
}

// --- [ RawDelim ] ------------------------------------------------------------

// from_untyped
func (RawDelim) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindRawDelim {
		return option.Some[AstNode](&RawDelim{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *RawDelim) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func RawDelim_default() *RawDelim {
	node := SyntaxNode_placeholder(SyntaxKindRawDelim)
	return &RawDelim{SyntaxNode: node}
}

// --- [ Link ] ----------------------------------------------------------------

// from_untyped
func (Link) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindLink {
		return option.Some[AstNode](&Link{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Link) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Link_default() *Link {
	node := SyntaxNode_placeholder(SyntaxKindLink)
	return &Link{SyntaxNode: node}
}

// --- [ Label ] ---------------------------------------------------------------

// from_untyped
func (Label) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindLabel {
		return option.Some[AstNode](&Label{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Label) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Label_default() *Label {
	node := SyntaxNode_placeholder(SyntaxKindLabel)
	return &Label{SyntaxNode: node}
}

// --- [ Ref ] -----------------------------------------------------------------

// from_untyped
func (Ref) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindRef {
		return option.Some[AstNode](&Ref{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Ref) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Ref_default() *Ref {
	node := SyntaxNode_placeholder(SyntaxKindRef)
	return &Ref{SyntaxNode: node}
}

// --- [ Heading ] -------------------------------------------------------------

// from_untyped
func (Heading) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindHeading {
		return option.Some[AstNode](&Heading{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Heading) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Heading_default() *Heading {
	node := SyntaxNode_placeholder(SyntaxKindHeading)
	return &Heading{SyntaxNode: node}
}

// --- [ ListItem ] ------------------------------------------------------------

// from_untyped
func (ListItem) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindListItem {
		return option.Some[AstNode](&ListItem{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ListItem) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ListItem_default() *ListItem {
	node := SyntaxNode_placeholder(SyntaxKindListItem)
	return &ListItem{SyntaxNode: node}
}

// --- [ EnumItem ] ------------------------------------------------------------

// from_untyped
func (EnumItem) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindEnumItem {
		return option.Some[AstNode](&EnumItem{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *EnumItem) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func EnumItem_default() *EnumItem {
	node := SyntaxNode_placeholder(SyntaxKindEnumItem)
	return &EnumItem{SyntaxNode: node}
}

// --- [ TermItem ] ------------------------------------------------------------

// from_untyped
func (TermItem) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindTermItem {
		return option.Some[AstNode](&TermItem{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *TermItem) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func TermItem_default() *TermItem {
	node := SyntaxNode_placeholder(SyntaxKindTermItem)
	return &TermItem{SyntaxNode: node}
}

// --- [ Equation ] ------------------------------------------------------------

// from_untyped
func (Equation) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindEquation {
		return option.Some[AstNode](&Equation{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Equation) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Equation_default() *Equation {
	node := SyntaxNode_placeholder(SyntaxKindEquation)
	return &Equation{SyntaxNode: node}
}

// --- [ Math ] ----------------------------------------------------------------

// from_untyped
func (Math) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMath {
		return option.Some[AstNode](&Math{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Math) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Math_default() *Math {
	node := SyntaxNode_placeholder(SyntaxKindMath)
	return &Math{SyntaxNode: node}
}

// --- [ MathText ] ------------------------------------------------------------

// from_untyped
func (MathText) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathText {
		return option.Some[AstNode](&MathText{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathText) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathText_default() *MathText {
	node := SyntaxNode_placeholder(SyntaxKindMathText)
	return &MathText{SyntaxNode: node}
}

// --- [ MathIdent ] -----------------------------------------------------------

// from_untyped
func (MathIdent) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathIdent {
		return option.Some[AstNode](&MathIdent{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathIdent) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathIdent_default() *MathIdent {
	node := SyntaxNode_placeholder(SyntaxKindMathIdent)
	return &MathIdent{SyntaxNode: node}
}

// --- [ MathShorthand ] -------------------------------------------------------

// from_untyped
func (MathShorthand) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathShorthand {
		return option.Some[AstNode](&MathShorthand{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathShorthand) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathShorthand_default() *MathShorthand {
	node := SyntaxNode_placeholder(SyntaxKindMathShorthand)
	return &MathShorthand{SyntaxNode: node}
}

// --- [ MathAlignPoint ] ------------------------------------------------------

// from_untyped
func (MathAlignPoint) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathAlignPoint {
		return option.Some[AstNode](&MathAlignPoint{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathAlignPoint) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathAlignPoint_default() *MathAlignPoint {
	node := SyntaxNode_placeholder(SyntaxKindMathAlignPoint)
	return &MathAlignPoint{SyntaxNode: node}
}

// --- [ MathDelimited ] -------------------------------------------------------

// from_untyped
func (MathDelimited) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathDelimited {
		return option.Some[AstNode](&MathDelimited{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathDelimited) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathDelimited_default() *MathDelimited {
	node := SyntaxNode_placeholder(SyntaxKindMathDelimited)
	return &MathDelimited{SyntaxNode: node}
}

// --- [ MathAttach ] ----------------------------------------------------------

// from_untyped
func (MathAttach) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathAttach {
		return option.Some[AstNode](&MathAttach{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathAttach) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathAttach_default() *MathAttach {
	node := SyntaxNode_placeholder(SyntaxKindMathAttach)
	return &MathAttach{SyntaxNode: node}
}

// --- [ MathPrimes ] ----------------------------------------------------------

// from_untyped
func (MathPrimes) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathPrimes {
		return option.Some[AstNode](&MathPrimes{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathPrimes) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathPrimes_default() *MathPrimes {
	node := SyntaxNode_placeholder(SyntaxKindMathPrimes)
	return &MathPrimes{SyntaxNode: node}
}

// --- [ MathFrac ] ------------------------------------------------------------

// from_untyped
func (MathFrac) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathFrac {
		return option.Some[AstNode](&MathFrac{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathFrac) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathFrac_default() *MathFrac {
	node := SyntaxNode_placeholder(SyntaxKindMathFrac)
	return &MathFrac{SyntaxNode: node}
}

// --- [ MathRoot ] ------------------------------------------------------------

// from_untyped
func (MathRoot) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindMathRoot {
		return option.Some[AstNode](&MathRoot{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *MathRoot) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func MathRoot_default() *MathRoot {
	node := SyntaxNode_placeholder(SyntaxKindMathRoot)
	return &MathRoot{SyntaxNode: node}
}

// --- [ Ident ] ---------------------------------------------------------------

// from_untyped
func (Ident) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindIdent {
		return option.Some[AstNode](&Ident{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Ident) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Ident_default() *Ident {
	node := SyntaxNode_placeholder(SyntaxKindIdent)
	return &Ident{SyntaxNode: node}
}

// --- [ None ] ----------------------------------------------------------------

// from_untyped
func (None) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindNone {
		return option.Some[AstNode](&None{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *None) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func None_default() *None {
	node := SyntaxNode_placeholder(SyntaxKindNone)
	return &None{SyntaxNode: node}
}

// --- [ Auto ] ----------------------------------------------------------------

// from_untyped
func (Auto) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindAuto {
		return option.Some[AstNode](&Auto{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Auto) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Auto_default() *Auto {
	node := SyntaxNode_placeholder(SyntaxKindAuto)
	return &Auto{SyntaxNode: node}
}

// --- [ Bool ] ----------------------------------------------------------------

// from_untyped
func (Bool) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindBool {
		return option.Some[AstNode](&Bool{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Bool) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Bool_default() *Bool {
	node := SyntaxNode_placeholder(SyntaxKindBool)
	return &Bool{SyntaxNode: node}
}

// --- [ Int ] -----------------------------------------------------------------

// from_untyped
func (Int) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindInt {
		return option.Some[AstNode](&Int{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Int) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Int_default() *Int {
	node := SyntaxNode_placeholder(SyntaxKindInt)
	return &Int{SyntaxNode: node}
}

// --- [ Float ] ---------------------------------------------------------------

// from_untyped
func (Float) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindFloat {
		return option.Some[AstNode](&Float{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Float) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Float_default() *Float {
	node := SyntaxNode_placeholder(SyntaxKindFloat)
	return &Float{SyntaxNode: node}
}

// --- [ Numeric ] -------------------------------------------------------------

// from_untyped
func (Numeric) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindNumeric {
		return option.Some[AstNode](&Numeric{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Numeric) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Numeric_default() *Numeric {
	node := SyntaxNode_placeholder(SyntaxKindNumeric)
	return &Numeric{SyntaxNode: node}
}

// --- [ Str ] -----------------------------------------------------------------

// from_untyped
func (Str) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindStr {
		return option.Some[AstNode](&Str{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Str) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Str_default() *Str {
	node := SyntaxNode_placeholder(SyntaxKindStr)
	return &Str{SyntaxNode: node}
}

// --- [ CodeBlock ] -----------------------------------------------------------

// from_untyped
func (CodeBlock) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindCodeBlock {
		return option.Some[AstNode](&CodeBlock{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *CodeBlock) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func CodeBlock_default() *CodeBlock {
	node := SyntaxNode_placeholder(SyntaxKindCodeBlock)
	return &CodeBlock{SyntaxNode: node}
}

// --- [ Code ] ----------------------------------------------------------------

// from_untyped
func (Code) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindCode {
		return option.Some[AstNode](&Code{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Code) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Code_default() *Code {
	node := SyntaxNode_placeholder(SyntaxKindCode)
	return &Code{SyntaxNode: node}
}

// --- [ ContentBlock ] --------------------------------------------------------

// from_untyped
func (ContentBlock) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindContentBlock {
		return option.Some[AstNode](&ContentBlock{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ContentBlock) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ContentBlock_default() *ContentBlock {
	node := SyntaxNode_placeholder(SyntaxKindContentBlock)
	return &ContentBlock{SyntaxNode: node}
}

// --- [ Parenthesized ] -------------------------------------------------------

// from_untyped
func (Parenthesized) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindParenthesized {
		return option.Some[AstNode](&Parenthesized{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Parenthesized) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Parenthesized_default() *Parenthesized {
	node := SyntaxNode_placeholder(SyntaxKindParenthesized)
	return &Parenthesized{SyntaxNode: node}
}

// --- [ Array ] ---------------------------------------------------------------

// from_untyped
func (Array) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindArray {
		return option.Some[AstNode](&Array{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Array) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Array_default() *Array {
	node := SyntaxNode_placeholder(SyntaxKindArray)
	return &Array{SyntaxNode: node}
}

// --- [ Dict ] ----------------------------------------------------------------

// from_untyped
func (Dict) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindDict {
		return option.Some[AstNode](&Dict{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Dict) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Dict_default() *Dict {
	node := SyntaxNode_placeholder(SyntaxKindDict)
	return &Dict{SyntaxNode: node}
}

// --- [ Named ] ---------------------------------------------------------------

// from_untyped
func (Named) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindNamed {
		return option.Some[AstNode](&Named{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Named) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Named_default() *Named {
	node := SyntaxNode_placeholder(SyntaxKindNamed)
	return &Named{SyntaxNode: node}
}

// --- [ Keyed ] ---------------------------------------------------------------

// from_untyped
func (Keyed) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindKeyed {
		return option.Some[AstNode](&Keyed{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Keyed) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Keyed_default() *Keyed {
	node := SyntaxNode_placeholder(SyntaxKindKeyed)
	return &Keyed{SyntaxNode: node}
}

// --- [ Spread ] --------------------------------------------------------------

// from_untyped
func (Spread) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSpread {
		return option.Some[AstNode](&Spread{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Spread) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Spread_default() *Spread {
	node := SyntaxNode_placeholder(SyntaxKindSpread)
	return &Spread{SyntaxNode: node}
}

// --- [ Unary ] ---------------------------------------------------------------

// from_untyped
func (Unary) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindUnary {
		return option.Some[AstNode](&Unary{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Unary) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Unary_default() *Unary {
	node := SyntaxNode_placeholder(SyntaxKindUnary)
	return &Unary{SyntaxNode: node}
}

// --- [ Binary ] --------------------------------------------------------------

// from_untyped
func (Binary) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindBinary {
		return option.Some[AstNode](&Binary{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Binary) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Binary_default() *Binary {
	node := SyntaxNode_placeholder(SyntaxKindBinary)
	return &Binary{SyntaxNode: node}
}

// --- [ FieldAccess ] ---------------------------------------------------------

// from_untyped
func (FieldAccess) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindFieldAccess {
		return option.Some[AstNode](&FieldAccess{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *FieldAccess) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func FieldAccess_default() *FieldAccess {
	node := SyntaxNode_placeholder(SyntaxKindFieldAccess)
	return &FieldAccess{SyntaxNode: node}
}

// --- [ FuncCall ] ------------------------------------------------------------

// from_untyped
func (FuncCall) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindFuncCall {
		return option.Some[AstNode](&FuncCall{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *FuncCall) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func FuncCall_default() *FuncCall {
	node := SyntaxNode_placeholder(SyntaxKindFuncCall)
	return &FuncCall{SyntaxNode: node}
}

// --- [ Args ] ----------------------------------------------------------------

// from_untyped
func (Args) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindArgs {
		return option.Some[AstNode](&Args{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Args) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Args_default() *Args {
	node := SyntaxNode_placeholder(SyntaxKindArgs)
	return &Args{SyntaxNode: node}
}

// --- [ Closure ] -------------------------------------------------------------

// from_untyped
func (Closure) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindClosure {
		return option.Some[AstNode](&Closure{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Closure) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Closure_default() *Closure {
	node := SyntaxNode_placeholder(SyntaxKindClosure)
	return &Closure{SyntaxNode: node}
}

// --- [ Params ] --------------------------------------------------------------

// from_untyped
func (Params) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindParams {
		return option.Some[AstNode](&Params{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Params) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Params_default() *Params {
	node := SyntaxNode_placeholder(SyntaxKindParams)
	return &Params{SyntaxNode: node}
}

// --- [ Underscore ] ----------------------------------------------------------

// from_untyped
func (Underscore) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindUnderscore {
		return option.Some[AstNode](&Underscore{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Underscore) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Underscore_default() *Underscore {
	node := SyntaxNode_placeholder(SyntaxKindUnderscore)
	return &Underscore{SyntaxNode: node}
}

// --- [ Destructuring ] -------------------------------------------------------

// from_untyped
func (Destructuring) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindDestructuring {
		return option.Some[AstNode](&Destructuring{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Destructuring) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Destructuring_default() *Destructuring {
	node := SyntaxNode_placeholder(SyntaxKindDestructuring)
	return &Destructuring{SyntaxNode: node}
}

// --- [ LetBinding ] ----------------------------------------------------------

// from_untyped
func (LetBinding) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindLetBinding {
		return option.Some[AstNode](&LetBinding{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *LetBinding) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func LetBinding_default() *LetBinding {
	node := SyntaxNode_placeholder(SyntaxKindLetBinding)
	return &LetBinding{SyntaxNode: node}
}

// --- [ DestructAssignment ] --------------------------------------------------

// from_untyped
func (DestructAssignment) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindDestructAssignment {
		return option.Some[AstNode](&DestructAssignment{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *DestructAssignment) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func DestructAssignment_default() *DestructAssignment {
	node := SyntaxNode_placeholder(SyntaxKindDestructAssignment)
	return &DestructAssignment{SyntaxNode: node}
}

// --- [ SetRule ] -------------------------------------------------------------

// from_untyped
func (SetRule) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSetRule {
		return option.Some[AstNode](&SetRule{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *SetRule) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func SetRule_default() *SetRule {
	node := SyntaxNode_placeholder(SyntaxKindSetRule)
	return &SetRule{SyntaxNode: node}
}

// --- [ ShowRule ] ------------------------------------------------------------

// from_untyped
func (ShowRule) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindShowRule {
		return option.Some[AstNode](&ShowRule{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ShowRule) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ShowRule_default() *ShowRule {
	node := SyntaxNode_placeholder(SyntaxKindShowRule)
	return &ShowRule{SyntaxNode: node}
}

// --- [ Contextual ] ----------------------------------------------------------

// from_untyped
func (Contextual) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindContextual {
		return option.Some[AstNode](&Contextual{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Contextual) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Contextual_default() *Contextual {
	node := SyntaxNode_placeholder(SyntaxKindContextual)
	return &Contextual{SyntaxNode: node}
}

// --- [ Conditional ] ---------------------------------------------------------

// from_untyped
func (Conditional) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindConditional {
		return option.Some[AstNode](&Conditional{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Conditional) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func Conditional_default() *Conditional {
	node := SyntaxNode_placeholder(SyntaxKindConditional)
	return &Conditional{SyntaxNode: node}
}

// --- [ WhileLoop ] -----------------------------------------------------------

// from_untyped
func (WhileLoop) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindWhileLoop {
		return option.Some[AstNode](&WhileLoop{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *WhileLoop) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func WhileLoop_default() *WhileLoop {
	node := SyntaxNode_placeholder(SyntaxKindWhileLoop)
	return &WhileLoop{SyntaxNode: node}
}

// --- [ ForLoop ] -------------------------------------------------------------

// from_untyped
func (ForLoop) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindForLoop {
		return option.Some[AstNode](&ForLoop{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ForLoop) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ForLoop_default() *ForLoop {
	node := SyntaxNode_placeholder(SyntaxKindForLoop)
	return &ForLoop{SyntaxNode: node}
}

// --- [ ModuleImport ] --------------------------------------------------------

// from_untyped
func (ModuleImport) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindModuleImport {
		return option.Some[AstNode](&ModuleImport{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ModuleImport) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ModuleImport_default() *ModuleImport {
	node := SyntaxNode_placeholder(SyntaxKindModuleImport)
	return &ModuleImport{SyntaxNode: node}
}

// --- [ ImportItems ] ---------------------------------------------------------

// from_untyped
func (ImportItems) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindImportItems {
		return option.Some[AstNode](&ImportItems{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ImportItems) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ImportItems_default() *ImportItems {
	node := SyntaxNode_placeholder(SyntaxKindImportItems)
	return &ImportItems{SyntaxNode: node}
}

// --- [ ImportItemPath ] ------------------------------------------------------

// from_untyped
func (ImportItemPath) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindImportItemPath {
		return option.Some[AstNode](&ImportItemPath{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ImportItemPath) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ImportItemPath_default() *ImportItemPath {
	node := SyntaxNode_placeholder(SyntaxKindImportItemPath)
	return &ImportItemPath{SyntaxNode: node}
}

// --- [ RenamedImportItem ] ---------------------------------------------------

// from_untyped
func (RenamedImportItem) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindRenamedImportItem {
		return option.Some[AstNode](&RenamedImportItem{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *RenamedImportItem) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func RenamedImportItem_default() *RenamedImportItem {
	node := SyntaxNode_placeholder(SyntaxKindRenamedImportItem)
	return &RenamedImportItem{SyntaxNode: node}
}

// --- [ ModuleInclude ] -------------------------------------------------------

// from_untyped
func (ModuleInclude) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindModuleInclude {
		return option.Some[AstNode](&ModuleInclude{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ModuleInclude) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func ModuleInclude_default() *ModuleInclude {
	node := SyntaxNode_placeholder(SyntaxKindModuleInclude)
	return &ModuleInclude{SyntaxNode: node}
}

// --- [ LoopBreak ] -----------------------------------------------------------

// from_untyped
func (LoopBreak) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindLoopBreak {
		return option.Some[AstNode](&LoopBreak{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *LoopBreak) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func LoopBreak_default() *LoopBreak {
	node := SyntaxNode_placeholder(SyntaxKindLoopBreak)
	return &LoopBreak{SyntaxNode: node}
}

// --- [ LoopContinue ] --------------------------------------------------------

// from_untyped
func (LoopContinue) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindLoopContinue {
		return option.Some[AstNode](&LoopContinue{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *LoopContinue) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func LoopContinue_default() *LoopContinue {
	node := SyntaxNode_placeholder(SyntaxKindLoopContinue)
	return &LoopContinue{SyntaxNode: node}
}

// --- [ FuncReturn ] ----------------------------------------------------------

// from_untyped
func (FuncReturn) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindFuncReturn {
		return option.Some[AstNode](&FuncReturn{SyntaxNode: node})
	} else {
		return option.None[AstNode]()
	}
}

func (node *FuncReturn) to_untyped() *SyntaxNode {
	return node.SyntaxNode
}

func FuncReturn_default() *FuncReturn {
	node := SyntaxNode_placeholder(SyntaxKindFuncReturn)
	return &FuncReturn{SyntaxNode: node}
}

// === [ ArrayItem ] ===========================================================

// --- [ ArrayItem_Pos ] -------------------------------------------------------

// from_untyped
func (ArrayItem_Pos) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if n, ok := SyntaxNode_cast[*ArrayItem_Pos](node).Get(); ok {
		return option.Some[AstNode](n)
	}
	return option.None[AstNode]()
}

func (node *ArrayItem_Pos) to_untyped() *SyntaxNode {
	return node.expr.to_untyped()
}

// --- [ ArrayItem_Spread ] ----------------------------------------------------

// from_untyped
func (ArrayItem_Spread) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSpread {
		return option.Some[AstNode](&ArrayItem_Spread{spread: &Spread{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *ArrayItem_Spread) to_untyped() *SyntaxNode {
	return node.spread.SyntaxNode
}

// === [ DictItem ] ============================================================

// --- [ DictItem_Named ] ------------------------------------------------------

// from_untyped
func (DictItem_Named) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindNamed {
		return option.Some[AstNode](&DictItem_Named{item: &Named{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *DictItem_Named) to_untyped() *SyntaxNode {
	return node.item.SyntaxNode
}

// --- [ DictItem_Keyed ] ------------------------------------------------------

// from_untyped
func (DictItem_Keyed) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindKeyed {
		return option.Some[AstNode](&DictItem_Keyed{item: &Keyed{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *DictItem_Keyed) to_untyped() *SyntaxNode {
	return node.item.SyntaxNode
}

// --- [ DictItem_Spread ] -----------------------------------------------------

// from_untyped
func (DictItem_Spread) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSpread {
		return option.Some[AstNode](&DictItem_Spread{item: &Spread{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *DictItem_Spread) to_untyped() *SyntaxNode {
	return node.item.SyntaxNode
}

// === [ Arg ] =================================================================

// --- [ Arg_Named ] -----------------------------------------------------------

// from_untyped
func (Arg_Named) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindNamed {
		return option.Some[AstNode](&Arg_Named{arg: &Named{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Arg_Named) to_untyped() *SyntaxNode {
	return node.arg.SyntaxNode
}

// --- [ Arg_Spread ] ----------------------------------------------------------

// from_untyped
func (Arg_Spread) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSpread {
		return option.Some[AstNode](&Arg_Spread{arg: &Spread{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Arg_Spread) to_untyped() *SyntaxNode {
	return node.arg.SyntaxNode
}

// --- [ Arg_Pos ] -------------------------------------------------------------

// from_untyped
func (Arg_Pos) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if n, ok := SyntaxNode_cast[*Arg_Pos](node).Get(); ok {
		return option.Some[AstNode](n)
	}
	return option.None[AstNode]()
}

func (node *Arg_Pos) to_untyped() *SyntaxNode {
	return node.arg.to_untyped()
}

// === [ Param ] ===============================================================

// --- [ Param_Named ] ---------------------------------------------------------

// from_untyped
func (Param_Named) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindNamed {
		return option.Some[AstNode](&Param_Named{param: &Named{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Param_Named) to_untyped() *SyntaxNode {
	return node.param.SyntaxNode
}

// --- [ Param_Spread ] --------------------------------------------------------

// from_untyped
func (Param_Spread) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSpread {
		return option.Some[AstNode](&Param_Spread{param: &Spread{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Param_Spread) to_untyped() *SyntaxNode {
	return node.param.SyntaxNode
}

// --- [ Param_Pos ] -----------------------------------------------------------

// from_untyped
func (Param_Pos) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if n, ok := SyntaxNode_cast[*Param_Pos](node).Get(); ok {
		return option.Some[AstNode](n)
	}
	return option.None[AstNode]()
}

func (node *Param_Pos) to_untyped() *SyntaxNode {
	return node.param.to_untyped()
}

// === [ Pattern ] ===============================================================

// --- [ Pattern_Normal ] ------------------------------------------------------

// from_untyped
func (Pattern_Normal) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if n, ok := SyntaxNode_cast[*Pattern_Normal](node).Get(); ok {
		return option.Some[AstNode](n)
	}
	return option.None[AstNode]()
}

func (node *Pattern_Normal) to_untyped() *SyntaxNode {
	return node.expr.to_untyped()
}

// Returns a list of all new bindings introduced by the pattern.
func (node *Pattern_Normal) bindings() []*Ident {
	if ident, ok := node.expr.(*Ident); ok {
		return []*Ident{ident}
	}
	return nil
}

func Pattern_Normal_default() *Pattern_Normal {
	return &Pattern_Normal{
		expr: Expr_default(),
	}
}

// --- [ Pattern_Placeholder ] -------------------------------------------------

// from_untyped
func (Pattern_Placeholder) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindUnderscore {
		return option.Some[AstNode](&Pattern_Placeholder{pattern: &Underscore{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Pattern_Placeholder) to_untyped() *SyntaxNode {
	return node.pattern.SyntaxNode
}

// Returns a list of all new bindings introduced by the pattern.
func (node *Pattern_Placeholder) bindings() []*Ident {
	return nil
}

// --- [ Pattern_Parenthesized ] -----------------------------------------------

// from_untyped
func (Pattern_Parenthesized) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindParenthesized {
		return option.Some[AstNode](&Pattern_Parenthesized{pattern: &Parenthesized{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Pattern_Parenthesized) to_untyped() *SyntaxNode {
	return node.pattern.SyntaxNode
}

// Returns a list of all new bindings introduced by the pattern.
func (node *Pattern_Parenthesized) bindings() []*Ident {
	return node.pattern.pattern().bindings()
}

// --- [ Pattern_Destructuring ] -----------------------------------------------

// from_untyped
func (Pattern_Destructuring) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindDestructuring {
		return option.Some[AstNode](&Pattern_Destructuring{pattern: &Destructuring{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *Pattern_Destructuring) to_untyped() *SyntaxNode {
	return node.pattern.SyntaxNode
}

// Returns a list of all new bindings introduced by the pattern.
func (node *Pattern_Destructuring) bindings() []*Ident {
	return node.pattern.bindings()
}

// === [ DestructuringItem ] ===================================================

// --- [ DestructuringItem_Pattern ] -------------------------------------------

// from_untyped
func (DestructuringItem_Pattern) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if n, ok := SyntaxNode_cast[*DestructuringItem_Pattern](node).Get(); ok {
		return option.Some[AstNode](n)
	}
	return option.None[AstNode]()
}

func (node *DestructuringItem_Pattern) to_untyped() *SyntaxNode {
	return node.pattern.to_untyped()
}

// --- [ DestructuringItem_Named ] ---------------------------------------------

// from_untyped
func (DestructuringItem_Named) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindNamed {
		return option.Some[AstNode](&DestructuringItem_Named{named: &Named{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *DestructuringItem_Named) to_untyped() *SyntaxNode {
	return node.named.SyntaxNode
}

// --- [ DestructuringItem_Spread ] ---------------------------------------------

// from_untyped
func (DestructuringItem_Spread) from_untyped(node *SyntaxNode) option.Option[AstNode] {
	if node.kind() == SyntaxKindSpread {
		return option.Some[AstNode](&DestructuringItem_Spread{spread: &Spread{SyntaxNode: node}})
	} else {
		return option.None[AstNode]()
	}
}

func (node *DestructuringItem_Spread) to_untyped() *SyntaxNode {
	return node.spread.SyntaxNode
}

package syntax

import (
	"strings"
)

// A syntax highlighting tag.
type Tag uint8

const (
	// A line or block comment.
	//
	// Comment
	Tag_Comment Tag = iota + 1
	// Punctuation in code.
	//
	// Punctuation
	Tag_Punctuation
	// An escape sequence or shorthand.
	//
	// Escape
	Tag_Escape
	// Strong markup.
	//
	// Strong
	Tag_Strong
	// Emphasized markup.
	//
	// Emph
	Tag_Emph
	// A hyperlink.
	//
	// Link
	Tag_Link
	// Raw text.
	//
	// Raw
	Tag_Raw
	// A label.
	//
	// Label
	Tag_Label
	// A reference to a label.
	//
	// Ref
	Tag_Ref
	// A section heading.
	//
	// Heading
	Tag_Heading
	// A marker of a list, enumeration, or term list.
	//
	// ListMarker
	Tag_ListMarker
	// A term in a term list.
	//
	// ListTerm
	Tag_ListTerm
	// The delimiters of an equation.
	//
	// MathDelimiter
	Tag_MathDelimiter
	// An operator with special meaning in an equation.
	//
	// MathOperator
	Tag_MathOperator
	// A keyword.
	//
	// Keyword
	Tag_Keyword
	// An operator in code.
	//
	// Operator
	Tag_Operator
	// A numeric literal.
	//
	// Number
	Tag_Number
	// A string literal.
	//
	// String
	Tag_String
	// A function or method name.
	//
	// Function
	Tag_Function
	// An interpolated variable in markup or math.
	//
	// Interpolated
	Tag_Interpolated
	// A syntax error.
	//
	// Error
	Tag_Error
)

// Return the recommended TextMate grammar scope for the given highlighting
// tag.
func (tag Tag) tm_scope() string {
	switch tag {
	case Tag_Comment:
		return "comment.typst"
	case Tag_Punctuation:
		return "punctuation.typst"
	case Tag_Escape:
		return "constant.character.escape.typst"
	case Tag_Strong:
		return "markup.bold.typst"
	case Tag_Emph:
		return "markup.italic.typst"
	case Tag_Link:
		return "markup.underline.link.typst"
	case Tag_Raw:
		return "markup.raw.typst"
	case Tag_MathDelimiter:
		return "punctuation.definition.math.typst"
	case Tag_MathOperator:
		return "keyword.operator.math.typst"
	case Tag_Heading:
		return "markup.heading.typst"
	case Tag_ListMarker:
		return "punctuation.definition.list.typst"
	case Tag_ListTerm:
		return "markup.list.term.typst"
	case Tag_Label:
		return "entity.name.label.typst"
	case Tag_Ref:
		return "markup.other.reference.typst"
	case Tag_Keyword:
		return "keyword.typst"
	case Tag_Operator:
		return "keyword.operator.typst"
	case Tag_Number:
		return "constant.numeric.typst"
	case Tag_String:
		return "string.quoted.double.typst"
	case Tag_Function:
		return "entity.name.function.typst"
	case Tag_Interpolated:
		return "meta.interpolation.typst"
	case Tag_Error:
		return "invalid.typst"
	}
	panic("unreachable")
}

// The recommended CSS class for the highlighting tag.
func (tag Tag) css_class() string {
	switch tag {
	case Tag_Comment:
		return "typ-comment"
	case Tag_Punctuation:
		return "typ-punct"
	case Tag_Escape:
		return "typ-escape"
	case Tag_Strong:
		return "typ-strong"
	case Tag_Emph:
		return "typ-emph"
	case Tag_Link:
		return "typ-link"
	case Tag_Raw:
		return "typ-raw"
	case Tag_Label:
		return "typ-label"
	case Tag_Ref:
		return "typ-ref"
	case Tag_Heading:
		return "typ-heading"
	case Tag_ListMarker:
		return "typ-marker"
	case Tag_ListTerm:
		return "typ-term"
	case Tag_MathDelimiter:
		return "typ-math-delim"
	case Tag_MathOperator:
		return "typ-math-op"
	case Tag_Keyword:
		return "typ-key"
	case Tag_Operator:
		return "typ-op"
	case Tag_Number:
		return "typ-num"
	case Tag_String:
		return "typ-str"
	case Tag_Function:
		return "typ-func"
	case Tag_Interpolated:
		return "typ-pol"
	case Tag_Error:
		return "typ-error"
	}
	panic("unreachable")
}

// Determine the highlight tag of a linked syntax node.
//
// Returns `None` if the node should not be highlighted.
func highlight(node *LinkedNode) (Tag, bool) {
	switch node.node.kind() {
	case SyntaxKindMarkup:
		if parent_kind, ok := node.parent_kind(); ok && parent_kind == SyntaxKindTermItem {
			if sibling_kind, ok := node.next_sibling_kind(); ok && sibling_kind == SyntaxKindColon {
				return Tag_ListTerm, true
			}
		}
		return 0, false
	case SyntaxKindText:
		return 0, false
	case SyntaxKindSpace:
		return 0, false
	case SyntaxKindLinebreak:
		return Tag_Escape, true
	case SyntaxKindParbreak:
		return 0, false
	case SyntaxKindEscape:
		return Tag_Escape, true
	case SyntaxKindShorthand:
		return Tag_Escape, true
	case SyntaxKindSmartQuote:
		return 0, false
	case SyntaxKindStrong:
		return Tag_Strong, true
	case SyntaxKindEmph:
		return Tag_Emph, true
	case SyntaxKindRaw:
		return Tag_Raw, true
	case SyntaxKindRawLang:
		return 0, false
	case SyntaxKindRawTrimmed:
		return 0, false
	case SyntaxKindRawDelim:
		return 0, false
	case SyntaxKindLink:
		return Tag_Link, true
	case SyntaxKindLabel:
		return Tag_Label, true
	case SyntaxKindRef:
		return Tag_Ref, true
	case SyntaxKindRefMarker:
		return 0, false
	case SyntaxKindHeading:
		return Tag_Heading, true
	case SyntaxKindHeadingMarker:
		return 0, false
	case SyntaxKindListItem:
		return 0, false
	case SyntaxKindListMarker:
		return Tag_ListMarker, true
	case SyntaxKindEnumItem:
		return 0, false
	case SyntaxKindEnumMarker:
		return Tag_ListMarker, true
	case SyntaxKindTermItem:
		return 0, false
	case SyntaxKindTermMarker:
		return Tag_ListMarker, true
	case SyntaxKindEquation:
		return 0, false

	case SyntaxKindMath:
		return 0, false
	case SyntaxKindMathText:
		return 0, false
	case SyntaxKindMathIdent:
		return highlight_ident(node)
	case SyntaxKindMathShorthand:
		return Tag_Escape, true
	case SyntaxKindMathAlignPoint:
		return Tag_MathOperator, true
	case SyntaxKindMathDelimited:
		return 0, false
	case SyntaxKindMathAttach:
		return 0, false
	case SyntaxKindMathFrac:
		return 0, false
	case SyntaxKindMathRoot:
		return 0, false
	case SyntaxKindMathPrimes:
		return 0, false

	case SyntaxKindHash:
		return highlight_hash(node)
	case SyntaxKindLeftBrace:
		return Tag_Punctuation, true
	case SyntaxKindRightBrace:
		return Tag_Punctuation, true
	case SyntaxKindLeftBracket:
		return Tag_Punctuation, true
	case SyntaxKindRightBracket:
		return Tag_Punctuation, true
	case SyntaxKindLeftParen:
		return Tag_Punctuation, true
	case SyntaxKindRightParen:
		return Tag_Punctuation, true
	case SyntaxKindComma:
		return Tag_Punctuation, true
	case SyntaxKindSemicolon:
		return Tag_Punctuation, true
	case SyntaxKindColon:
		return Tag_Punctuation, true
	case SyntaxKindStar:
		if parent_kind, ok := node.parent_kind(); ok && parent_kind == SyntaxKindStrong {
			return 0, false
		}
		return Tag_Operator, true
	case SyntaxKindUnderscore:
		if parent_kind, ok := node.parent_kind(); ok && parent_kind == SyntaxKindMathAttach {
			return Tag_MathOperator, true
		}
		return 0, false
	case SyntaxKindDollar:
		return Tag_MathDelimiter, true
	case SyntaxKindPlus:
		return Tag_Operator, true
	case SyntaxKindMinus:
		return Tag_Operator, true
	case SyntaxKindSlash:
		if parent_kind, ok := node.parent_kind(); ok && parent_kind == SyntaxKindMathFrac {
			return Tag_MathOperator, true
		}
		return Tag_Operator, true
	case SyntaxKindHat:
		return Tag_MathOperator, true
	case SyntaxKindPrime:
		return Tag_MathOperator, true
	case SyntaxKindDot:
		return Tag_Punctuation, true
	case SyntaxKindEq:
		if parent_kind, ok := node.parent_kind(); ok && parent_kind == SyntaxKindHeading {
			return 0, false
		}
		return Tag_Operator, true
	case SyntaxKindEqEq:
		return Tag_Operator, true
	case SyntaxKindExclEq:
		return Tag_Operator, true
	case SyntaxKindLt:
		return Tag_Operator, true
	case SyntaxKindLtEq:
		return Tag_Operator, true
	case SyntaxKindGt:
		return Tag_Operator, true
	case SyntaxKindGtEq:
		return Tag_Operator, true
	case SyntaxKindPlusEq:
		return Tag_Operator, true
	case SyntaxKindHyphEq:
		return Tag_Operator, true
	case SyntaxKindStarEq:
		return Tag_Operator, true
	case SyntaxKindSlashEq:
		return Tag_Operator, true
	case SyntaxKindDots:
		return Tag_Operator, true
	case SyntaxKindArrow:
		return Tag_Operator, true
	case SyntaxKindRoot:
		return Tag_MathOperator, true

	case SyntaxKindNot:
		return Tag_Keyword, true
	case SyntaxKindAnd:
		return Tag_Keyword, true
	case SyntaxKindOr:
		return Tag_Keyword, true
	case SyntaxKindNone:
		return Tag_Keyword, true
	case SyntaxKindAuto:
		return Tag_Keyword, true
	case SyntaxKindLet:
		return Tag_Keyword, true
	case SyntaxKindSet:
		return Tag_Keyword, true
	case SyntaxKindShow:
		return Tag_Keyword, true
	case SyntaxKindContext:
		return Tag_Keyword, true
	case SyntaxKindIf:
		return Tag_Keyword, true
	case SyntaxKindElse:
		return Tag_Keyword, true
	case SyntaxKindFor:
		return Tag_Keyword, true
	case SyntaxKindIn:
		return Tag_Keyword, true
	case SyntaxKindWhile:
		return Tag_Keyword, true
	case SyntaxKindBreak:
		return Tag_Keyword, true
	case SyntaxKindContinue:
		return Tag_Keyword, true
	case SyntaxKindReturn:
		return Tag_Keyword, true
	case SyntaxKindImport:
		return Tag_Keyword, true
	case SyntaxKindInclude:
		return Tag_Keyword, true
	case SyntaxKindAs:
		return Tag_Keyword, true

	case SyntaxKindCode:
		return 0, false
	case SyntaxKindIdent:
		return highlight_ident(node)
	case SyntaxKindBool:
		return Tag_Keyword, true
	case SyntaxKindInt:
		return Tag_Number, true
	case SyntaxKindFloat:
		return Tag_Number, true
	case SyntaxKindNumeric:
		return Tag_Number, true
	case SyntaxKindStr:
		return Tag_String, true
	case SyntaxKindCodeBlock:
		return 0, false
	case SyntaxKindContentBlock:
		return 0, false
	case SyntaxKindParenthesized:
		return 0, false
	case SyntaxKindArray:
		return 0, false
	case SyntaxKindDict:
		return 0, false
	case SyntaxKindNamed:
		return 0, false
	case SyntaxKindKeyed:
		return 0, false
	case SyntaxKindUnary:
		return 0, false
	case SyntaxKindBinary:
		return 0, false
	case SyntaxKindFieldAccess:
		return 0, false
	case SyntaxKindFuncCall:
		return 0, false
	case SyntaxKindArgs:
		return 0, false
	case SyntaxKindSpread:
		return 0, false
	case SyntaxKindClosure:
		return 0, false
	case SyntaxKindParams:
		return 0, false
	case SyntaxKindLetBinding:
		return 0, false
	case SyntaxKindSetRule:
		return 0, false
	case SyntaxKindShowRule:
		return 0, false
	case SyntaxKindContextual:
		return 0, false
	case SyntaxKindConditional:
		return 0, false
	case SyntaxKindWhileLoop:
		return 0, false
	case SyntaxKindForLoop:
		return 0, false
	case SyntaxKindModuleImport:
		return 0, false
	case SyntaxKindImportItems:
		return 0, false
	case SyntaxKindImportItemPath:
		return 0, false
	case SyntaxKindRenamedImportItem:
		return 0, false
	case SyntaxKindModuleInclude:
		return 0, false
	case SyntaxKindLoopBreak:
		return 0, false
	case SyntaxKindLoopContinue:
		return 0, false
	case SyntaxKindFuncReturn:
		return 0, false
	case SyntaxKindDestructuring:
		return 0, false
	case SyntaxKindDestructAssignment:
		return 0, false

	case SyntaxKindShebang:
		return Tag_Comment, true
	case SyntaxKindLineComment:
		return Tag_Comment, true
	case SyntaxKindBlockComment:
		return Tag_Comment, true
	case SyntaxKindError:
		return Tag_Error, true
	case SyntaxKindEnd:
		return 0, false
	}
	panic("unreachable")
}

// Highlight an identifier based on context.
func highlight_ident(node *LinkedNode) (Tag, bool) {
	// Are we directly before an argument list?
	if next, ok := node.next_leaf(); ok {
		if uint(node.Range().End) == next.offset {
			switch next.node.kind() {
			case SyntaxKindLeftParen:
				if parent_kind, ok := next.parent_kind(); ok {
					if parent_kind == SyntaxKindArgs || parent_kind == SyntaxKindParams {
						return Tag_Function, true
					}
				}
			case SyntaxKindLeftBracket:
				if parent_kind, ok := next.parent_kind(); ok {
					if parent_kind == SyntaxKindContentBlock {
						return Tag_Function, true
					}
				}
			}
		}
	}

	// Are we in math?
	if node.node.kind() == SyntaxKindMathIdent {
		return Tag_Interpolated, true
	}

	// Find the first non-field access ancestor.
	ancestor := node
	for {
		if parent_kind, ok := ancestor.parent_kind(); ok && parent_kind == SyntaxKindFieldAccess {
			ancestor = ancestor.parent.MustGet()
		} else {
			break
		}
	}

	// Are we directly before or behind a show rule colon?
	if parent_kind, ok := ancestor.parent_kind(); ok {
		if parent_kind == SyntaxKindShowRule {
			if next, ok := node.next_leaf(); ok {
				if next.node.kind() == SyntaxKindColon {
					return Tag_Function, true
				}
			}
			if prev, ok := node.prev_leaf(); ok {
				if prev.node.kind() == SyntaxKindColon {
					return Tag_Function, true
				}
			}
		}
	}

	// Are we (or an ancestor field access) directly after a hash.
	if prev, ok := ancestor.prev_leaf(); ok {
		if prev.node.kind() == SyntaxKindHash {
			return Tag_Interpolated, true
		}
	}

	// Are we behind a dot, that is behind another identifier?
	if prev, ok := node.prev_leaf(); ok {
		if prev.node.kind() == SyntaxKindDot {
			if prev_prev, ok := prev.prev_leaf(); ok {
				if Node_is_ident(prev_prev) {
					return highlight_ident(prev_prev)
				}
			}
		}
	}

	return 0, false
}

// Highlight a hash based on context.
func highlight_hash(node *LinkedNode) (Tag, bool) {
	next, ok := node.next_sibling()
	if !ok {
		return 0, false
	}
	expr := SyntaxNode_cast[Expr](next.node).MustGet()
	if !Expr_hash(expr) {
		return 0, false
	}
	if left, ok := next.leftmost_leaf(); ok {
		return highlight(left)
	}
	return 0, false
}

// Whether the node is one of the two identifier nodes.
//
// is_ident
func Node_is_ident(node *LinkedNode) bool {
	switch node.node.kind() {
	case SyntaxKindIdent, SyntaxKindMathIdent:
		return true
	}
	return false
}

// Highlight a node to an HTML `code` element.
//
// This uses these [CSS classes for categories](Tag_css_class).
func highlight_html(root *SyntaxNode) string {
	buf := &strings.Builder{}
	buf.WriteString("<code>")
	node := NewLinkedNode(root)
	highlight_html_impl(buf, node)
	buf.WriteString("</code>")
	return buf.String()
}

// Highlight one source node, emitting HTML.
func highlight_html_impl(html *strings.Builder, node *LinkedNode) {
	span := false
	if tag, ok := highlight(node); ok && tag != Tag_Error {
		span = true
		html.WriteString("<span class=\"")
		html.WriteString(tag.css_class())
		html.WriteString("\">")
	}

	text := node.node.text()
	if len(text) > 0 {
		for _, c := range text {
			switch c {
			case '<':
				html.WriteString("&lt;")
			case '>':
				html.WriteString("&gt;")
			case '&':
				html.WriteString("&amp;")
			case '\'':
				html.WriteString("&#39;")
			case '"':
				html.WriteString("&quot;")
			default:
				html.WriteRune(c)
			}
		}
	} else {
		for _, child := range node.children().Children() {
			highlight_html_impl(html, child)
		}
	}

	if span {
		html.WriteString("</span>")
	}
}

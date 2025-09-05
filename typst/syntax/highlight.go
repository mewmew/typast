package syntax

import "github.com/mewmew/typast/internal/option"

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
func highlight(node *LinkedNode) option.Option[Tag] {
	switch node.node.kind() {
	case SyntaxKindMarkup:
		if parent_kind, ok := node.parent_kind().Get(); ok && parent_kind == SyntaxKindTermItem {
			if sibling_kind, ok := node.next_sibling_kind().Get(); ok && sibling_kind == SyntaxKindColon {
				return option.Some(Tag_ListTerm)
			}
		}
		return option.None[Tag]()
	case SyntaxKindText:
		return option.None[Tag]()
	case SyntaxKindSpace:
		return option.None[Tag]()
	case SyntaxKindLinebreak:
		return option.Some(Tag_Escape)
	case SyntaxKindParbreak:
		return option.None[Tag]()
	case SyntaxKindEscape:
		return option.Some(Tag_Escape)
	case SyntaxKindShorthand:
		return option.Some(Tag_Escape)
	case SyntaxKindSmartQuote:
		return option.None[Tag]()
	case SyntaxKindStrong:
		return option.Some(Tag_Strong)
	case SyntaxKindEmph:
		return option.Some(Tag_Emph)
	case SyntaxKindRaw:
		return option.Some(Tag_Raw)
	case SyntaxKindRawLang:
		return option.None[Tag]()
	case SyntaxKindRawTrimmed:
		return option.None[Tag]()
	case SyntaxKindRawDelim:
		return option.None[Tag]()
	case SyntaxKindLink:
		return option.Some(Tag_Link)
	case SyntaxKindLabel:
		return option.Some(Tag_Label)
	case SyntaxKindRef:
		return option.Some(Tag_Ref)
	case SyntaxKindRefMarker:
		return option.None[Tag]()
	case SyntaxKindHeading:
		return option.Some(Tag_Heading)
	case SyntaxKindHeadingMarker:
		return option.None[Tag]()
	case SyntaxKindListItem:
		return option.None[Tag]()
	case SyntaxKindListMarker:
		return option.Some(Tag_ListMarker)
	case SyntaxKindEnumItem:
		return option.None[Tag]()
	case SyntaxKindEnumMarker:
		return option.Some(Tag_ListMarker)
	case SyntaxKindTermItem:
		return option.None[Tag]()
	case SyntaxKindTermMarker:
		return option.Some(Tag_ListMarker)
	case SyntaxKindEquation:
		return option.None[Tag]()

	case SyntaxKindMath:
		return option.None[Tag]()
	case SyntaxKindMathText:
		return option.None[Tag]()
	case SyntaxKindMathIdent:
		return highlight_ident(node)
	case SyntaxKindMathShorthand:
		return option.Some(Tag_Escape)
	case SyntaxKindMathAlignPoint:
		return option.Some(Tag_MathOperator)
	case SyntaxKindMathDelimited:
		return option.None[Tag]()
	case SyntaxKindMathAttach:
		return option.None[Tag]()
	case SyntaxKindMathFrac:
		return option.None[Tag]()
	case SyntaxKindMathRoot:
		return option.None[Tag]()
	case SyntaxKindMathPrimes:
		return option.None[Tag]()

	case SyntaxKindHash:
		return highlight_hash(node)
	case SyntaxKindLeftBrace:
		return option.Some(Tag_Punctuation)
	case SyntaxKindRightBrace:
		return option.Some(Tag_Punctuation)
	case SyntaxKindLeftBracket:
		return option.Some(Tag_Punctuation)
	case SyntaxKindRightBracket:
		return option.Some(Tag_Punctuation)
	case SyntaxKindLeftParen:
		return option.Some(Tag_Punctuation)
	case SyntaxKindRightParen:
		return option.Some(Tag_Punctuation)
	case SyntaxKindComma:
		return option.Some(Tag_Punctuation)
	case SyntaxKindSemicolon:
		return option.Some(Tag_Punctuation)
	case SyntaxKindColon:
		return option.Some(Tag_Punctuation)
	case SyntaxKindStar:
		if parent_kind, ok := node.parent_kind().Get(); ok && parent_kind == SyntaxKindStrong {
			return option.None[Tag]()
		}
		return option.Some(Tag_Operator)
	case SyntaxKindUnderscore:
		if parent_kind, ok := node.parent_kind().Get(); ok && parent_kind == SyntaxKindMathAttach {
			return option.Some(Tag_MathOperator)
		}
		return option.None[Tag]()
	case SyntaxKindDollar:
		return option.Some(Tag_MathDelimiter)
	case SyntaxKindPlus:
		return option.Some(Tag_Operator)
	case SyntaxKindMinus:
		return option.Some(Tag_Operator)
	case SyntaxKindSlash:
		if parent_kind, ok := node.parent_kind().Get(); ok && parent_kind == SyntaxKindMathFrac {
			return option.Some(Tag_MathOperator)
		}
		return option.Some(Tag_Operator)
	case SyntaxKindHat:
		return option.Some(Tag_MathOperator)
	case SyntaxKindPrime:
		return option.Some(Tag_MathOperator)
	case SyntaxKindDot:
		return option.Some(Tag_Punctuation)
	case SyntaxKindEq:
		if parent_kind, ok := node.parent_kind().Get(); ok && parent_kind == SyntaxKindHeading {
			return option.None[Tag]()
		}
		return option.Some(Tag_Operator)
	case SyntaxKindEqEq:
		return option.Some(Tag_Operator)
	case SyntaxKindExclEq:
		return option.Some(Tag_Operator)
	case SyntaxKindLt:
		return option.Some(Tag_Operator)
	case SyntaxKindLtEq:
		return option.Some(Tag_Operator)
	case SyntaxKindGt:
		return option.Some(Tag_Operator)
	case SyntaxKindGtEq:
		return option.Some(Tag_Operator)
	case SyntaxKindPlusEq:
		return option.Some(Tag_Operator)
	case SyntaxKindHyphEq:
		return option.Some(Tag_Operator)
	case SyntaxKindStarEq:
		return option.Some(Tag_Operator)
	case SyntaxKindSlashEq:
		return option.Some(Tag_Operator)
	case SyntaxKindDots:
		return option.Some(Tag_Operator)
	case SyntaxKindArrow:
		return option.Some(Tag_Operator)
	case SyntaxKindRoot:
		return option.Some(Tag_MathOperator)

	case SyntaxKindNot:
		return option.Some(Tag_Keyword)
	case SyntaxKindAnd:
		return option.Some(Tag_Keyword)
	case SyntaxKindOr:
		return option.Some(Tag_Keyword)
	case SyntaxKindNone:
		return option.Some(Tag_Keyword)
	case SyntaxKindAuto:
		return option.Some(Tag_Keyword)
	case SyntaxKindLet:
		return option.Some(Tag_Keyword)
	case SyntaxKindSet:
		return option.Some(Tag_Keyword)
	case SyntaxKindShow:
		return option.Some(Tag_Keyword)
	case SyntaxKindContext:
		return option.Some(Tag_Keyword)
	case SyntaxKindIf:
		return option.Some(Tag_Keyword)
	case SyntaxKindElse:
		return option.Some(Tag_Keyword)
	case SyntaxKindFor:
		return option.Some(Tag_Keyword)
	case SyntaxKindIn:
		return option.Some(Tag_Keyword)
	case SyntaxKindWhile:
		return option.Some(Tag_Keyword)
	case SyntaxKindBreak:
		return option.Some(Tag_Keyword)
	case SyntaxKindContinue:
		return option.Some(Tag_Keyword)
	case SyntaxKindReturn:
		return option.Some(Tag_Keyword)
	case SyntaxKindImport:
		return option.Some(Tag_Keyword)
	case SyntaxKindInclude:
		return option.Some(Tag_Keyword)
	case SyntaxKindAs:
		return option.Some(Tag_Keyword)

	case SyntaxKindCode:
		return option.None[Tag]()
	case SyntaxKindIdent:
		return highlight_ident(node)
	case SyntaxKindBool:
		return option.Some(Tag_Keyword)
	case SyntaxKindInt:
		return option.Some(Tag_Number)
	case SyntaxKindFloat:
		return option.Some(Tag_Number)
	case SyntaxKindNumeric:
		return option.Some(Tag_Number)
	case SyntaxKindStr:
		return option.Some(Tag_String)
	case SyntaxKindCodeBlock:
		return option.None[Tag]()
	case SyntaxKindContentBlock:
		return option.None[Tag]()
	case SyntaxKindParenthesized:
		return option.None[Tag]()
	case SyntaxKindArray:
		return option.None[Tag]()
	case SyntaxKindDict:
		return option.None[Tag]()
	case SyntaxKindNamed:
		return option.None[Tag]()
	case SyntaxKindKeyed:
		return option.None[Tag]()
	case SyntaxKindUnary:
		return option.None[Tag]()
	case SyntaxKindBinary:
		return option.None[Tag]()
	case SyntaxKindFieldAccess:
		return option.None[Tag]()
	case SyntaxKindFuncCall:
		return option.None[Tag]()
	case SyntaxKindArgs:
		return option.None[Tag]()
	case SyntaxKindSpread:
		return option.None[Tag]()
	case SyntaxKindClosure:
		return option.None[Tag]()
	case SyntaxKindParams:
		return option.None[Tag]()
	case SyntaxKindLetBinding:
		return option.None[Tag]()
	case SyntaxKindSetRule:
		return option.None[Tag]()
	case SyntaxKindShowRule:
		return option.None[Tag]()
	case SyntaxKindContextual:
		return option.None[Tag]()
	case SyntaxKindConditional:
		return option.None[Tag]()
	case SyntaxKindWhileLoop:
		return option.None[Tag]()
	case SyntaxKindForLoop:
		return option.None[Tag]()
	case SyntaxKindModuleImport:
		return option.None[Tag]()
	case SyntaxKindImportItems:
		return option.None[Tag]()
	case SyntaxKindImportItemPath:
		return option.None[Tag]()
	case SyntaxKindRenamedImportItem:
		return option.None[Tag]()
	case SyntaxKindModuleInclude:
		return option.None[Tag]()
	case SyntaxKindLoopBreak:
		return option.None[Tag]()
	case SyntaxKindLoopContinue:
		return option.None[Tag]()
	case SyntaxKindFuncReturn:
		return option.None[Tag]()
	case SyntaxKindDestructuring:
		return option.None[Tag]()
	case SyntaxKindDestructAssignment:
		return option.None[Tag]()

	case SyntaxKindShebang:
		return option.Some(Tag_Comment)
	case SyntaxKindLineComment:
		return option.Some(Tag_Comment)
	case SyntaxKindBlockComment:
		return option.Some(Tag_Comment)
	case SyntaxKindError:
		return option.Some(Tag_Error)
	case SyntaxKindEnd:
		return option.None[Tag]()
	}
	panic("unreachable")
}

// Highlight an identifier based on context.
func highlight_ident(node *LinkedNode) option.Option[Tag] {
	// Are we directly before an argument list?
	next_leaf := node.next_leaf()
	if next, ok := next_leaf.Get(); ok {
		if uint(node._range().End) == next.offset {
			switch next.node.kind() {
			case SyntaxKindLeftParen:
				if parent_kind, ok := next.parent_kind().Get(); ok {
					if parent_kind == SyntaxKindArgs || parent_kind == SyntaxKindParams {
						return option.Some(Tag_Function)
					}
				}
			case SyntaxKindLeftBracket:
				if parent_kind, ok := next.parent_kind().Get(); ok {
					if parent_kind == SyntaxKindContentBlock {
						return option.Some(Tag_Function)
					}
				}
			}
		}
	}

	// Are we in math?
	if node.node.kind() == SyntaxKindMathIdent {
		return option.Some(Tag_Interpolated)
	}

	// Find the first non-field access ancestor.
	ancestor := node
	for {
		if parent_kind, ok := ancestor.parent_kind().Get(); ok && parent_kind == SyntaxKindFieldAccess {
			ancestor = ancestor.parent.MustGet()
		} else {
			break
		}
	}

	// Are we directly before or behind a show rule colon?
	if parent_kind, ok := ancestor.parent_kind().Get(); ok {
		if parent_kind == SyntaxKindShowRule {
			if next, ok := next_leaf.Get(); ok {
				if next.node.kind() == SyntaxKindColon {
					return option.Some(Tag_Function)
				}
			}
			if prev, ok := node.prev_leaf().Get(); ok {
				if prev.node.kind() == SyntaxKindColon {
					return option.Some(Tag_Function)
				}
			}
		}
	}


	// Are we (or an ancestor field access) directly after a hash.
	if prev, ok := ancestor.prev_leaf().Get(); ok {
		if prev.node.kind() == SyntaxKindHash {
			return option.Some(Tag_Interpolated)
		}
	}

	// Are we behind a dot, that is behind another identifier?
	prev, ok := node.prev_leaf().Get()
	if !ok {
		return option.None[Tag]()
	}
	if prev.node.kind() == SyntaxKindDot {
		prev_prev := prev.prev_leaf().MustGet()
		if Node_is_ident(prev_prev) {
			return highlight_ident(prev_prev)
		}
	}

	return option.None[Tag]()
}

// Highlight a hash based on context.
func highlight_hash(node *LinkedNode) option.Option[Tag] {
	next := node.next_sibling().MustGet()
	expr := SyntaxNode_cast[Expr](next.node).MustGet()
	if !Expr_hash(expr) {
		return option.None[Tag]()
	}
	return highlight(next.leftmost_leaf().MustGet())
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

/*
// Highlight a node to an HTML `code` element.
//
// This uses these [CSS classes for categories](Tag_css_class).
pub fn highlight_html(root: &SyntaxNode) -> String {
	let mut buf = String::from("<code>");
	let node = LinkedNode::new(root);
	highlight_html_impl(&mut buf, &node);
	buf.push_str("</code>");
	buf
}

// Highlight one source node, emitting HTML.
fn highlight_html_impl(html: &mut String, node: &LinkedNode) {
	let mut span = false;
	if let Some(tag) = highlight(node)
		&& tag != Tag_Error
	{
		span = true;
		html.push_str("<span class=\"");
		html.push_str(tag.css_class());
		html.push_str("\">");
	}

	let text = node.text();
	if !text.is_empty() {
		for c in text.chars() {
			match c {
				'<' => html.push_str("&lt;"),
				'>' => html.push_str("&gt;"),
				'&' => html.push_str("&amp;"),
				'\'' => html.push_str("&#39;"),
				'"' => html.push_str("&quot;"),
				_ => html.push(c),
			}
		}
	} else {
		for child in node.children() {
			highlight_html_impl(html, &child);
		}
	}

	if span {
		html.push_str("</span>");
	}
}
*/

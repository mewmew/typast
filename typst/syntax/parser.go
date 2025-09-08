package syntax

import (
	"fmt"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
	"github.com/mewmew/typast/internal/stdx"
	"github.com/mewmew/typast/internal/typutil"
	"github.com/mewmew/typast/internal/vector"
)

// Parses a source file as top-level markup.
func Parse(text string) *SyntaxNode {
	p := NewParser(text, 0, SyntaxModeMarkup)
	markup_exprs(p, true, NewSyntaxSet(SyntaxKindEnd))
	return p.finish_into(SyntaxKindMarkup)
}

// Parses top-level code.
func parse_code(text string) *SyntaxNode {
	p := NewParser(text, 0, SyntaxModeCode)
	code_exprs(p, NewSyntaxSet(SyntaxKindEnd))
	return p.finish_into(SyntaxKindCode)
}

// Parses top-level math.
func parse_math(text string) *SyntaxNode {
	p := NewParser(text, 0, SyntaxModeMath)
	math_exprs(p, NewSyntaxSet(SyntaxKindEnd))
	return p.finish_into(SyntaxKindMath)
}

// Parses markup expressions until a stop condition is met.
func markup(p *Parser, at_start, wrap_trivia bool, stop_set *SyntaxSet) {
	var m Marker
	if wrap_trivia {
		m = p.before_trivia()
	} else {
		m = p.marker()
	}
	markup_exprs(p, at_start, stop_set)
	if wrap_trivia {
		p.flush_trivia()
	}
	p.wrap(m, SyntaxKindMarkup)
}

// Parses a sequence of markup expressions.
func markup_exprs(p *Parser, at_start bool, stop_set *SyntaxSet) {
	if !stop_set.Contains(SyntaxKindEnd) {
		panic("stop set missing end token")
	}
	if p.had_newline() {
		at_start = true
	}
	nesting := uint(0)
	// Keep going if we're at a nested right-bracket regardless of the stop set.
	for !p.at_set(stop_set) || (nesting > 0 && p.at(SyntaxKindRightBracket)) {
		markup_expr(p, at_start, &nesting)
		at_start = p.had_newline()
	}
}

// Reparses a subsection of markup incrementally.
func reparse_markup(text string, _range ranges.Range, at_start *bool, nesting *uint, top_level bool) ([]*SyntaxNode, bool) {
	p := NewParser(text, uint(_range.Start), SyntaxModeMarkup)
	if p.had_newline() {
		*at_start = true
	}
	for !p.end() && p.current_start() < uint(_range.End) {
		// If not top-level and at a new RightBracket, stop the reparse.
		if !top_level && *nesting == 0 && p.at(SyntaxKindRightBracket) {
			break
		}
		markup_expr(p, *at_start, nesting)
		*at_start = p.had_newline()
	}
	if p.balanced && p.current_start() == uint(_range.End) {
		return p.finish(), true
	}
	return nil, false
}

// Parses a single markup expression. This includes markup elements like text,
// headings, strong/emph, lists/enums, etc. This is also the entry point for
// parsing math equations and embedded code expressions.
func markup_expr(p *Parser, at_start bool, nesting *uint) {
	switch p.current() {
	case SyntaxKindLeftBracket:
		*nesting++
		p.convert_and_eat(SyntaxKindText)
	case SyntaxKindRightBracket:
		if *nesting > 0 {
			*nesting--
			p.convert_and_eat(SyntaxKindText)
		} else {
			p.unexpected()
			p.hint("try using a backslash escape: \\]")
		}

	case SyntaxKindShebang:
		p.eat()

	case SyntaxKindText, SyntaxKindLinebreak, SyntaxKindEscape, SyntaxKindShorthand, SyntaxKindSmartQuote, SyntaxKindLink, SyntaxKindLabel:
		p.eat()

	case SyntaxKindRaw:
		p.eat() // Raw is handled entirely in the Lexer.

	case SyntaxKindHash:
		embedded_code_expr(p)
	case SyntaxKindStar:
		strong(p)
	case SyntaxKindUnderscore:
		emph(p)
	case SyntaxKindHeadingMarker:
		if at_start {
			heading(p)
		} else {
			p.convert_and_eat(SyntaxKindText)
		}
	case SyntaxKindListMarker:
		if at_start {
			list_item(p)
		} else {
			p.convert_and_eat(SyntaxKindText)
		}
	case SyntaxKindEnumMarker:
		if at_start {
			enum_item(p)
		} else {
			p.convert_and_eat(SyntaxKindText)
		}
	case SyntaxKindTermMarker:
		if at_start {
			term_item(p)
		} else {
			p.convert_and_eat(SyntaxKindText)
		}
	case SyntaxKindRefMarker:
		reference(p)
	case SyntaxKindDollar:
		equation(p)

	case SyntaxKindColon:
		p.convert_and_eat(SyntaxKindText)

	default:
		p.unexpected()
	}
}

// Parses strong content: `*Strong*`.
func strong(p *Parser) {
	p.with_nl_mode(&AtNewline_StopParBreak{}, func(p *Parser) {
		m := p.marker()
		p.assert(SyntaxKindStar)
		markup(p, false, true, NewSyntaxSet(SyntaxKindStar, SyntaxKindRightBracket, SyntaxKindEnd))
		p.expect_closing_delimiter(m, SyntaxKindStar)
		p.wrap(m, SyntaxKindStrong)
	})
}

// Parses emphasized content: `_Emphasized_`.
func emph(p *Parser) {
	p.with_nl_mode(&AtNewline_StopParBreak{}, func(p *Parser) {
		m := p.marker()
		p.assert(SyntaxKindUnderscore)
		markup(p, false, true, NewSyntaxSet(SyntaxKindUnderscore, SyntaxKindRightBracket, SyntaxKindEnd))
		p.expect_closing_delimiter(m, SyntaxKindUnderscore)
		p.wrap(m, SyntaxKindEmph)
	})
}

// Parses a section heading: `= Introduction`.
func heading(p *Parser) {
	p.with_nl_mode(&AtNewline_Stop{}, func(p *Parser) {
		m := p.marker()
		p.assert(SyntaxKindHeadingMarker)
		markup(p, false, false, NewSyntaxSet(SyntaxKindLabel, SyntaxKindRightBracket, SyntaxKindEnd))
		p.wrap(m, SyntaxKindHeading)
	})
}

// Parses an item in a bullet list: `- ...`.
func list_item(p *Parser) {
	p.with_nl_mode(NewAtNewline_RequireColumn(p.current_column()), func(p *Parser) {
		m := p.marker()
		p.assert(SyntaxKindListMarker)
		markup(p, true, false, NewSyntaxSet(SyntaxKindRightBracket, SyntaxKindEnd))
		p.wrap(m, SyntaxKindListItem)
	})
}

// Parses an item in an enumeration (numbered list): `+ ...` or `1. ...`.
func enum_item(p *Parser) {
	p.with_nl_mode(NewAtNewline_RequireColumn(p.current_column()), func(p *Parser) {
		m := p.marker()
		p.assert(SyntaxKindEnumMarker)
		markup(p, true, false, NewSyntaxSet(SyntaxKindRightBracket, SyntaxKindEnd))
		p.wrap(m, SyntaxKindEnumItem)
	})
}

// Parses an item in a term list: `/ Term: Details`.
func term_item(p *Parser) {
	p.with_nl_mode(NewAtNewline_RequireColumn(p.current_column()), func(p *Parser) {
		m := p.marker()
		p.with_nl_mode(&AtNewline_Stop{}, func(p *Parser) {
			p.assert(SyntaxKindTermMarker)
			markup(p, false, false, NewSyntaxSet(SyntaxKindColon, SyntaxKindRightBracket, SyntaxKindEnd))
		})
		p.expect(SyntaxKindColon)
		markup(p, true, false, NewSyntaxSet(SyntaxKindRightBracket, SyntaxKindEnd))
		p.wrap(m, SyntaxKindTermItem)
	})
}

// Parses a reference: `@target`, `@target[..]`.
func reference(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindRefMarker)
	if p.directly_at(SyntaxKindLeftBracket) {
		content_block(p)
	}
	p.wrap(m, SyntaxKindRef)
}

// Parses a mathematical equation: `$x$`, `$ x^2 $`.
func equation(p *Parser) {
	m := p.marker()
	p.enter_modes(SyntaxModeMath, &AtNewline_Continue{}, func(p *Parser) {
		p.assert(SyntaxKindDollar)
		math_(p, NewSyntaxSet(SyntaxKindDollar, SyntaxKindEnd))
		p.expect_closing_delimiter(m, SyntaxKindDollar)
	})
	p.wrap(m, SyntaxKindEquation)
}

// Parses the contents of a mathematical equation: `x^2 + 1`.
//
// math
func math_(p *Parser, stop_set *SyntaxSet) {
	m := p.marker()
	math_exprs(p, stop_set)
	p.wrap(m, SyntaxKindMath)
}

// Parses a sequence of math expressions. Returns the number of expressions
// parsed.
func math_exprs(p *Parser, stop_set *SyntaxSet) uint {
	if !stop_set.Contains(SyntaxKindEnd) {
		panic("stop set missing end token")
	}
	count := uint(0)
	for !p.at_set(stop_set) {
		if p.at_set(MathExprSet) {
			math_expr(p)
			count++
		} else {
			p.unexpected()
		}
	}
	return count
}

// Parses a single math expression: This includes math elements like
// attachment, fractions, and roots, and embedded code expressions.
func math_expr(p *Parser) {
	math_expr_prec(p, 0, SyntaxKindEnd)
}

// Parses a math expression with at least the given precedence.
func math_expr_prec(p *Parser, min_prec uint, stop SyntaxKind) {
	m := p.marker()
	continuable := false
	switch p.current() {
	case SyntaxKindHash:
		embedded_code_expr(p)
	case SyntaxKindMathIdent, SyntaxKindFieldAccess:
		// The lexer manages creating full FieldAccess nodes if needed.
		continuable = true
		p.eat()
		// Parse a function call for an identifier or field access.
		if min_prec < 3 && p.directly_at(SyntaxKindMathText) && p.current_text() == "(" {
			math_args(p)
			p.wrap(m, SyntaxKindFuncCall)
			continuable = false
		}

	case SyntaxKindDot, SyntaxKindComma, SyntaxKindSemicolon, SyntaxKindRightParen:
		p.convert_and_eat(SyntaxKindMathText)

	case SyntaxKindText, SyntaxKindMathText, SyntaxKindMathShorthand:
		// `a(b)/c` parses as `(a(b))/c` if `a` is continuable.
		if class, ok := math_class(p.current_text()).Get(); ok && class == stdx.MathClass_Alphabetic {
			continuable = true
		} else if all(p.current_text(), stdx.IsAlphabetic) {
			continuable = true
		}

		if !maybe_delimited(p) {
			p.eat()
		}

	case SyntaxKindLinebreak, SyntaxKindMathAlignPoint:
		p.eat()
	case SyntaxKindEscape, SyntaxKindStr:
		continuable = true
		p.eat()

	case SyntaxKindRoot:
		if min_prec < 3 {
			p.eat()
			m2 := p.marker()
			math_expr_prec(p, 2, stop)
			math_unparen(p, m2)
			p.wrap(m, SyntaxKindMathRoot)
		}

	case SyntaxKindPrime:
		// Means that there is nothing to attach the prime to.
		continuable = true
		for p.at(SyntaxKindPrime) {
			m2 := p.marker()
			p.eat()
			// Eat the group until the space.
			for p.eat_if_direct(SyntaxKindPrime) {
				// do nothing
			}
			p.wrap(m2, SyntaxKindMathPrimes)
		}
	default:
		p.expected("expression")
	}

	if continuable && min_prec < 3 && !p.had_trivia() && maybe_delimited(p) {
		p.wrap(m, SyntaxKindMath)
	}

	// Whether there were _any_ primes in the loop.
	primed := false

	for !p.end() && !p.at(stop) {
		if p.directly_at(SyntaxKindMathText) && p.current_text() == "!" {
			p.eat()
			p.wrap(m, SyntaxKindMath)
			continue
		}

		prime_marker := p.marker()
		if p.eat_if_direct(SyntaxKindPrime) {
			// Eat as many primes as possible.
			for p.eat_if_direct(SyntaxKindPrime) {
				// do nothing
			}
			p.wrap(prime_marker, SyntaxKindMathPrimes)

			// Will not be continued, so need to wrap the prime as attachment.
			if p.at(stop) {
				p.wrap(m, SyntaxKindMathAttach)
			}

			primed = true
			continue
		}

		op, ok := math_op(p.current()).Get()
		if !ok {
			// No attachments, so we need to wrap primes as attachment.
			if primed {
				p.wrap(m, SyntaxKindMathAttach)
			}

			break
		}

		if primed && op.kind == SyntaxKindMathFrac {
			p.wrap(m, SyntaxKindMathAttach)
		}

		prec := op.prec
		if prec < min_prec {
			break
		}

		switch op.assoc {
		case Assoc_Left:
			prec++
		case Assoc_Right:
			// do nothing
		}

		if op.kind == SyntaxKindMathFrac {
			math_unparen(p, m)
		}

		p.eat()
		m2 := p.marker()
		math_expr_prec(p, prec, op.stop)
		math_unparen(p, m2)

		if p.eat_if(SyntaxKindUnderscore) || p.eat_if(SyntaxKindHat) {
			m3 := p.marker()
			math_expr_prec(p, prec, SyntaxKindEnd)
			math_unparen(p, m3)
		}

		p.wrap(m, op.kind)
	}
}

type MathOp struct {
	kind  SyntaxKind
	stop  SyntaxKind
	assoc Assoc
	prec  uint
}

// Precedence and wrapper kinds for the binary math operators.
func math_op(kind SyntaxKind) option.Option[MathOp] {
	switch kind {
	case SyntaxKindUnderscore:
		return option.Some(MathOp{
			kind:  SyntaxKindMathAttach,
			stop:  SyntaxKindHat,
			assoc: Assoc_Right,
			prec:  3,
		})
	case SyntaxKindHat:
		return option.Some(MathOp{
			kind:  SyntaxKindMathAttach,
			stop:  SyntaxKindUnderscore,
			assoc: Assoc_Right,
			prec:  3,
		})
	case SyntaxKindSlash:
		return option.Some(MathOp{
			kind:  SyntaxKindMathFrac,
			stop:  SyntaxKindEnd,
			assoc: Assoc_Left,
			prec:  1,
		})
	default:
		return option.None[MathOp]()
	}
}

// Try to parse delimiters based on the current token's unicode math class.
func maybe_delimited(p *Parser) bool {
	open := false
	if class, ok := math_class(p.current_text()).Get(); ok && class == stdx.MathClass_Opening {
		open = true
	}
	if open {
		math_delimited(p)
	}
	return open
}

// Parse matched delimiters in math: `[x + y]`.
func math_delimited(p *Parser) {
	m := p.marker()
	p.eat()
	m2 := p.marker()
	for !p.at_set(NewSyntaxSet(SyntaxKindDollar, SyntaxKindEnd)) {
		if class, ok := math_class(p.current_text()).Get(); ok && class == stdx.MathClass_Closing {
			p.wrap(m2, SyntaxKindMath)
			// We could be at the shorthand `|]`, which shouldn't be converted
			// to a `Text` kind.
			if p.at(SyntaxKindRightParen) {
				p.convert_and_eat(SyntaxKindMathText)
			} else {
				p.eat()
			}
			p.wrap(m, SyntaxKindMathDelimited)
			return
		}

		if p.at_set(MathExprSet) {
			math_expr(p)
		} else {
			p.unexpected()
		}
	}

	p.wrap(m, SyntaxKindMath)
}

// Remove one set of parentheses (if any) from a previously parsed expression
// by converting to non-expression SyntaxKinds.
func math_unparen(p *Parser, m Marker) {
	if int(m) >= len(p.nodes) {
		return
	}
	node := p.nodes[m]
	if node.kind() != SyntaxKindMathDelimited {
		return
	}

	children := node.children()
	if len(children) < 1 {
		return
	}
	first := children[0]
	last := children[len(children)-1]
	if first.text() == "(" && last.text() == ")" {
		first.convert_to_kind(SyntaxKindLeftParen)
		last.convert_to_kind(SyntaxKindRightParen)
		// Only convert if we did have regular parens.
		node.convert_to_kind(SyntaxKindMath)
	}
}

// The unicode math class of a string. Only returns `Some` if `text` has
// exactly one unicode character or is a math shorthand string (currently just
// `[|`, `||`, `|]`) and then only returns `Some` if there is a math class
// defined for that character.
func math_class(text string) option.Option[stdx.MathClass] {
	switch text {
	case "[|":
		return option.Some(stdx.MathClass_Opening)
	case "|]":
		return option.Some(stdx.MathClass_Closing)
	case "||":
		return option.Some(stdx.MathClass_Fence)
	}

	for _, c := range text {
		return typutil.DefaultMathClass(c)
	}
	return option.None[stdx.MathClass]()
}

// Parse an argument list in math: `(a, b; c, d; size: #50%)`.
func math_args(p *Parser) {
	m := p.marker()
	p.convert_and_eat(SyntaxKindLeftParen)

	positional := true
	has_arrays := false

	maybe_array_start := p.marker()
	seen := make(map[string]bool)
	for !p.at_set(NewSyntaxSet(SyntaxKindEnd, SyntaxKindDollar, SyntaxKindRightParen)) {
		positional = math_arg(p, seen)

		switch p.current() {
		case SyntaxKindComma:
			p.eat()
			if !positional {
				maybe_array_start = p.marker()
			}
		case SyntaxKindSemicolon:
			if !positional {
				maybe_array_start = p.marker()
			}

			// Parses an array: `a, b, c;`.
			// The semicolon merges preceding arguments separated by commas
			// into an array argument.
			p.wrap(maybe_array_start, SyntaxKindArray)
			p.eat()
			maybe_array_start = p.marker()
			has_arrays = true
		case SyntaxKindEnd, SyntaxKindDollar, SyntaxKindRightParen:
			// do nothing
		default:
			p.expected("comma or semicolon")
		}
	}

	// Check if we need to wrap the preceding arguments in an array.
	if maybe_array_start != p.marker() && has_arrays && positional {
		p.wrap(maybe_array_start, SyntaxKindArray)
	}

	p.expect_closing_delimiter(m, SyntaxKindRightParen)
	p.wrap(m, SyntaxKindArgs)
}

// Parses a single argument in a math argument list.
//
// Returns whether the parsed argument was positional or not.
func math_arg(p *Parser, seen map[string]bool) bool {
	m := p.marker()
	start := p.current_start()

	if p.at(SyntaxKindDot) {
		// Parses a spread argument: `..args`.
		if spread, ok := p.lexer.maybe_math_spread_arg(start).Get(); ok {
			p.token.node = spread
			p.eat()
			math_expr(p)
			p.wrap(m, SyntaxKindSpread)
			return true
		}
	}

	positional := true
	if p.at_set(NewSyntaxSet(SyntaxKindMathText, SyntaxKindMathIdent, SyntaxKindUnderscore)) {
		// Parses a named argument: `thickness: #12pt`.
		if named, ok := p.lexer.maybe_math_named_arg(start).Get(); ok {
			p.token.node = named
			text := p.current_text()
			p.eat()
			p.convert_and_eat(SyntaxKindColon)
			if !map_insert(seen, text) {
				p.index(m).convert_to_error(fmt.Sprintf("duplicate argument: %v", text))
			}
			positional = false
		}
	}

	// Parses a normal positional argument.
	arg := p.marker()
	count := math_exprs(p, NewSyntaxSet(SyntaxKindEnd, SyntaxKindDollar, SyntaxKindComma, SyntaxKindSemicolon, SyntaxKindRightParen))
	if count == 0 {
		// Named argument requires a value.
		if !positional {
			p.expected("expression")
		}

		// Flush trivia so that the new empty Math node will be wrapped _inside_
		// any `SyntaxKindArray` elements created in `math_args`.
		// (And if we don't follow by wrapping in an array, it has no effect.)
		// The difference in node layout without this would look like:
		// Expression: `$ mat( ;) $`
		// - Correct:   [ .., Space(" "), Array[Math[], ], Semicolon(";"), .. ]
		// - Incorrect: [ .., Math[], Array[], Space(" "), Semicolon(";"), .. ]
		p.flush_trivia()
	}

	// Wrap math function arguments to join adjacent math content or create an
	// empty 'Math' node for when we have 0 args. We don't wrap when
	// `count == 1`, since wrapping would change the type of the expression
	// from potentially non-content to content. Ex: `$ func(#12pt) $` would
	// change the type from size to content if wrapped.
	if count != 1 {
		p.wrap(arg, SyntaxKindMath)
	}

	if !positional {
		p.wrap(m, SyntaxKindNamed)
	}
	return positional
}

// Parses the contents of a code block.
func code(p *Parser, stop_set *SyntaxSet) {
	m := p.marker()
	code_exprs(p, stop_set)
	p.wrap(m, SyntaxKindCode)
}

// Parses a sequence of code expressions.
func code_exprs(p *Parser, stop_set *SyntaxSet) {
	if !stop_set.Contains(SyntaxKindEnd) {
		panic("stop set missing end token")
	}
	for !p.at_set(stop_set) {
		p.with_nl_mode(&AtNewline_ContextualContinue{}, func(p *Parser) {
			if !p.at_set(CodeExprSet) {
				p.unexpected()
				return
			}
			code_expr(p)
			if !p.at_set(stop_set) && !p.eat_if(SyntaxKindSemicolon) {
				p.expected("semicolon or line break")
				if p.at(SyntaxKindLabel) {
					p.hint("labels can only be applied in markup mode")
					p.hint("try wrapping your code in a markup block (`[ ]`)")
				}
			}
		})
	}
}

// Parses an atomic code expression embedded in markup or math.
func embedded_code_expr(p *Parser) {
	p.enter_modes(SyntaxModeCode, &AtNewline_Stop{}, func(p *Parser) {
		p.assert(SyntaxKindHash)
		if p.had_trivia() || p.end() {
			p.expected("expression")
			return
		}

		stmt := p.at_set(StmtSet)
		at := p.at_set(AtomicCodeExprSet)
		code_expr_prec(p, true, 0)

		// Consume error for things like `#12p` or `#"abc\"`.#
		if !at {
			p.unexpected()
		}

		semi := (stmt || p.directly_at(SyntaxKindSemicolon)) && p.eat_if(SyntaxKindSemicolon)

		if stmt && !semi && !p.end() && !p.at(SyntaxKindRightBracket) {
			p.expected("semicolon or line break")
		}
	})
}

// Parses a single code expression.
func code_expr(p *Parser) {
	code_expr_prec(p, false, 0)
}

// Parses a code expression with at least the given precedence.
func code_expr_prec(p *Parser, atomic bool, min_prec uint) {
	m := p.marker()
	if !atomic && p.at_set(UnaryOpSet) {
		op := UnOp_from_kind(p.current()).MustGet()
		p.eat()
		code_expr_prec(p, atomic, op.precedence())
		p.wrap(m, SyntaxKindUnary)
	} else {
		code_primary(p, atomic)
	}

	for {
		if p.directly_at(SyntaxKindLeftParen) || p.directly_at(SyntaxKindLeftBracket) {
			args(p)
			p.wrap(m, SyntaxKindFuncCall)
			continue
		}

		at_field_or_method := false
		if p.directly_at(SyntaxKindDot) {
			if kind, _ := p.lexer.clone().next(); kind == SyntaxKindIdent {
				at_field_or_method = true
			}
		}

		if atomic && !at_field_or_method {
			break
		}

		if p.eat_if(SyntaxKindDot) {
			p.expect(SyntaxKindIdent)
			p.wrap(m, SyntaxKindFieldAccess)
			continue
		}

		var binop option.Option[BinOp]
		if p.at_set(BinaryOpSet) {
			binop = BinOp_from_kind(p.current())
		} else if min_prec <= BinOp_NotIn.precedence() && p.eat_if(SyntaxKindNot) {
			if p.at(SyntaxKindIn) {
				binop = option.Some(BinOp_NotIn)
			} else {
				p.expected("keyword `in`")
				break
			}
		} else {
			binop = option.None[BinOp]()
		}

		if op, ok := binop.Get(); ok {
			prec := op.precedence()
			if prec < min_prec {
				break
			}

			switch op.assoc() {
			case Assoc_Left:
				prec++
			case Assoc_Right:
				// do nothing
			}

			p.eat()
			code_expr_prec(p, false, prec)
			p.wrap(m, SyntaxKindBinary)
			continue
		}

		break
	}
}

// Parses an primary in a code expression. These are the atoms that unary and
// binary operations, functions calls, and field accesses start with / are
// composed of.
func code_primary(p *Parser, atomic bool) {
	m := p.marker()
	switch p.current() {
	case SyntaxKindIdent:
		p.eat()
		if !atomic && p.at(SyntaxKindArrow) {
			p.wrap(m, SyntaxKindParams)
			p.assert(SyntaxKindArrow)
			code_expr(p)
			p.wrap(m, SyntaxKindClosure)
		}
	case SyntaxKindUnderscore:
		if !atomic {
			p.eat()
			if p.at(SyntaxKindArrow) {
				p.wrap(m, SyntaxKindParams)
				p.eat()
				code_expr(p)
				p.wrap(m, SyntaxKindClosure)
			} else if p.eat_if(SyntaxKindEq) {
				code_expr(p)
				p.wrap(m, SyntaxKindDestructAssignment)
			} else {
				p.index(m).expected("expression")
			}
		} else {
			p.expected("expression")
		}

	case SyntaxKindLeftBrace:
		code_block(p)
	case SyntaxKindLeftBracket:
		content_block(p)
	case SyntaxKindLeftParen:
		expr_with_paren(p, atomic)
	case SyntaxKindDollar:
		equation(p)
	case SyntaxKindLet:
		let_binding(p)
	case SyntaxKindSet:
		set_rule(p)
	case SyntaxKindShow:
		show_rule(p)
	case SyntaxKindContext:
		contextual(p, atomic)
	case SyntaxKindIf:
		conditional(p)
	case SyntaxKindWhile:
		while_loop(p)
	case SyntaxKindFor:
		for_loop(p)
	case SyntaxKindImport:
		module_import(p)
	case SyntaxKindInclude:
		module_include(p)
	case SyntaxKindBreak:
		break_stmt(p)
	case SyntaxKindContinue:
		continue_stmt(p)
	case SyntaxKindReturn:
		return_stmt(p)

	case SyntaxKindRaw:
		p.eat() // Raw is handled entirely in the Lexer.

	case SyntaxKindNone, SyntaxKindAuto, SyntaxKindInt, SyntaxKindFloat, SyntaxKindBool, SyntaxKindNumeric, SyntaxKindStr, SyntaxKindLabel:
		p.eat()

	default:
		p.expected("expression")
	}
}

// Reparses a full content or code block.
func reparse_block(text string, _range ranges.Range) option.Option[*SyntaxNode] {
	p := NewParser(text, uint(_range.Start), SyntaxModeCode)
	if !(p.at(SyntaxKindLeftBracket) || p.at(SyntaxKindLeftBrace)) {
		panic(fmt.Sprintf("expected left bracket or left brace"))
	}
	block(p)
	if p.balanced && p.prev_end() == uint(_range.End) {
		for _, node := range p.finish() {
			return option.Some(node)
		}
	}
	return option.None[*SyntaxNode]()
}

// Parses a content or code block.
func block(p *Parser) {
	switch p.current() {
	case SyntaxKindLeftBracket:
		content_block(p)
	case SyntaxKindLeftBrace:
		code_block(p)
	default:
		p.expected("block")
	}
}

// Parses a code block: `{ let x = 1; x + 2 }`.
func code_block(p *Parser) {
	m := p.marker()
	p.enter_modes(SyntaxModeCode, &AtNewline_Continue{}, func(p *Parser) {
		p.assert(SyntaxKindLeftBrace)
		code(p, NewSyntaxSet(SyntaxKindRightBrace, SyntaxKindRightBracket, SyntaxKindRightParen, SyntaxKindEnd))
		p.expect_closing_delimiter(m, SyntaxKindRightBrace)
	})
	p.wrap(m, SyntaxKindCodeBlock)
}

// Parses a content block: `[*Hi* there!]`.
func content_block(p *Parser) {
	m := p.marker()
	p.enter_modes(SyntaxModeMarkup, &AtNewline_Continue{}, func(p *Parser) {
		p.assert(SyntaxKindLeftBracket)
		markup(p, true, true, NewSyntaxSet(SyntaxKindRightBracket, SyntaxKindEnd))
		p.expect_closing_delimiter(m, SyntaxKindRightBracket)
	})
	p.wrap(m, SyntaxKindContentBlock)
}

// Parses a let binding: `let x = 1`.
func let_binding(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindLet)

	m2 := p.marker()
	closure := false
	other := false

	if p.eat_if(SyntaxKindIdent) {
		if p.directly_at(SyntaxKindLeftParen) {
			params(p)
			closure = true
		}
	} else {
		pattern(p, false, make(map[string]bool), option.None[string]())
		other = true
	}

	if closure || other {
		if p.expect(SyntaxKindEq) {
			code_expr(p)
		}
	} else {
		if p.eat_if(SyntaxKindEq) {
			code_expr(p)
		}
	}

	if closure {
		p.wrap(m2, SyntaxKindClosure)
	}

	p.wrap(m, SyntaxKindLetBinding)
}

// Parses a set rule: `set text(...)`.
func set_rule(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindSet)

	m2 := p.marker()
	p.expect(SyntaxKindIdent)
	for p.eat_if(SyntaxKindDot) {
		p.expect(SyntaxKindIdent)
		p.wrap(m2, SyntaxKindFieldAccess)
	}

	args(p)
	if p.eat_if(SyntaxKindIf) {
		code_expr(p)
	}
	p.wrap(m, SyntaxKindSetRule)
}

// Parses a show rule: `show heading: it => emph(it.body)`.
func show_rule(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindShow)
	m2 := p.before_trivia()

	if !p.at(SyntaxKindColon) {
		code_expr(p)
	}

	if p.eat_if(SyntaxKindColon) {
		code_expr(p)
	} else {
		p.expected_at(m2, "colon")
	}

	p.wrap(m, SyntaxKindShowRule)
}

// Parses a contextual expression: `context text.lang`.
func contextual(p *Parser, atomic bool) {
	m := p.marker()
	p.assert(SyntaxKindContext)
	code_expr_prec(p, atomic, 0)
	p.wrap(m, SyntaxKindContextual)
}

// Parses an if-else conditional: `if x { y } else { z }`.
func conditional(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindIf)
	code_expr(p)
	block(p)
	if p.eat_if(SyntaxKindElse) {
		if p.at(SyntaxKindIf) {
			conditional(p)
		} else {
			block(p)
		}
	}
	p.wrap(m, SyntaxKindConditional)
}

// Parses a while loop: `while x { y }`.
func while_loop(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindWhile)
	code_expr(p)
	block(p)
	p.wrap(m, SyntaxKindWhileLoop)
}

// Parses a for loop: `for x in y { z }`.
func for_loop(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindFor)

	seen := make(map[string]bool)
	pattern(p, false, seen, option.None[string]())

	if p.at(SyntaxKindComma) {
		node := p.eat_and_get()
		node.unexpected()
		node.hint("destructuring patterns must be wrapped in parentheses")
		if p.at_set(PatternSet) {
			pattern(p, false, seen, option.None[string]())
		}
	}

	p.expect(SyntaxKindIn)
	code_expr(p)
	block(p)
	p.wrap(m, SyntaxKindForLoop)
}

// Parses a module import: `import "utils.typ": a, b, c`.
func module_import(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindImport)
	code_expr(p)
	if p.eat_if(SyntaxKindAs) {
		// Allow renaming a full module import.
		// If items are included, both the full module and the items are
		// imported at the same time.
		p.expect(SyntaxKindIdent)
	}

	if p.eat_if(SyntaxKindColon) {
		if p.at(SyntaxKindLeftParen) {
			p.with_nl_mode(&AtNewline_Continue{}, func(p *Parser) {
				m2 := p.marker()
				p.assert(SyntaxKindLeftParen)

				import_items(p)

				p.expect_closing_delimiter(m2, SyntaxKindRightParen)
			})
		} else if !p.eat_if(SyntaxKindStar) {
			import_items(p)
		}
	}

	p.wrap(m, SyntaxKindModuleImport)
}

// Parses items to import from a module: `a, b, c`.
func import_items(p *Parser) {
	m := p.marker()
	for !p.current().IsTerminator() {
		item_marker := p.marker()
		if !p.eat_if(SyntaxKindIdent) {
			p.unexpected()
		}

		// Nested import path: `a.b.c`
		for p.eat_if(SyntaxKindDot) {
			p.expect(SyntaxKindIdent)
		}

		p.wrap(item_marker, SyntaxKindImportItemPath)

		// Rename imported item.
		if p.eat_if(SyntaxKindAs) {
			p.expect(SyntaxKindIdent)
			p.wrap(item_marker, SyntaxKindRenamedImportItem)
		}

		if !p.current().IsTerminator() {
			p.expect(SyntaxKindComma)
		}
	}

	p.wrap(m, SyntaxKindImportItems)
}

// Parses a module include: `include "chapter1.typ"`.
func module_include(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindInclude)
	code_expr(p)
	p.wrap(m, SyntaxKindModuleInclude)
}

// Parses a break from a loop: `break`.
func break_stmt(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindBreak)
	p.wrap(m, SyntaxKindLoopBreak)
}

// Parses a continue in a loop: `continue`.
func continue_stmt(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindContinue)
	p.wrap(m, SyntaxKindLoopContinue)
}

// Parses a return from a function: `return`, `return x + 1`.
func return_stmt(p *Parser) {
	m := p.marker()
	p.assert(SyntaxKindReturn)
	if p.at_set(CodeExprSet) {
		code_expr(p)
	}
	p.wrap(m, SyntaxKindFuncReturn)
}

// An expression that starts with a parenthesis.
func expr_with_paren(p *Parser, atomic bool) {
	if atomic {
		// Atomic expressions aren't modified by operators that follow them, so
		// our first guess of array/dict will be correct.
		parenthesized_or_array_or_dict(p)
		return
	}

	// If we've seen this position before and have a memoized result, restore it
	// and return. Otherwise, get a key to this position and a checkpoint to
	// restart from in case we make a wrong prediction.
	memo_checkpoint, ok := p.restore_memo_or_checkpoint().Get()
	if !ok {
		return
	}
	memo_key := memo_checkpoint.memo_key
	checkpoint := memo_checkpoint.checkpoint
	// The node length from when we restored.
	prev_len := checkpoint.node_len

	// When we reach a '(', we can't be sure what it is. First, we attempt to
	// parse as a simple parenthesized expression, array, or dictionary as
	// these are the most likely things. We can handle all of those in a single
	// pass.
	kind := parenthesized_or_array_or_dict(p)

	// If, however, '=>' or '=' follows, we must backtrack and reparse as either
	// a parameter list or a destructuring. To be able to do that, we created a
	// parser checkpoint before our speculative parse, which we can restore.
	//
	// However, naive backtracking has a fatal flaw: It can lead to exponential
	// parsing time if we are constantly getting things wrong in a nested
	// scenario. The particular failure case for parameter parsing is the
	// following: `(x: (x: (x) => y) => y) => y`
	//
	// Such a structure will reparse over and over again recursively, leading to
	// a running time of O(2^n) for nesting depth n. To prevent this, we perform
	// a simple trick: When we have done the mistake of picking the wrong path
	// once and have subsequently parsed correctly, we save the result of that
	// correct parsing in the `p.memo` map. When we reach the same position
	// again, we can then just restore this result. In this way, no
	// parenthesized expression is parsed more than twice, leading to a worst
	// case running time of O(2n).
	if p.at(SyntaxKindArrow) {
		p.restore(checkpoint)
		m := p.marker()
		params(p)
		if !p.expect(SyntaxKindArrow) {
			return
		}
		code_expr(p)
		p.wrap(m, SyntaxKindClosure)
	} else if p.at(SyntaxKindEq) && kind != SyntaxKindParenthesized {
		p.restore(checkpoint)
		m := p.marker()
		destructuring_or_parenthesized(p, true, make(map[string]bool))
		if !p.expect(SyntaxKindEq) {
			return
		}
		code_expr(p)
		p.wrap(m, SyntaxKindDestructAssignment)
	} else {
		return
	}

	// Memoize result if we backtracked.
	p.memoize_parsed_nodes(memo_key, prev_len)
}

// Parses either
// - a parenthesized expression: `(1 + 2)`, or
// - an array: `(1, "hi", 12cm)`, or
// - a dictionary: `(thickness: 3pt, dash: "solid")`.
func parenthesized_or_array_or_dict(p *Parser) SyntaxKind {
	state := &GroupState{
		count:             0,
		maybe_just_parens: true,
		kind:              option.None[SyntaxKind](),
		seen:              make(map[string]bool),
	}

	// An edge case with parens is whether we can interpret a leading spread
	// expression as a dictionary, e.g. if we want `(..dict1, ..dict2)` to join
	// the two dicts.
	//
	// The issue is that we decide on the type of the parenthesized expression
	// here in the parser by the `SyntaxKind` we wrap with, instead of in eval
	// based on the type of the spread item.
	//
	// The current fix is that we allow a leading colon to force the
	// parenthesized value into a dict:
	// - `(..arr1, ..arr2)` is wrapped as an `Array`.
	// - `(: ..dict1, ..dict2)` is wrapped as a `Dict`.
	//
	// This does allow some unexpected expressions, such as `(: key: val)`, but
	// it's currently intentional.
	m := p.marker()
	p.with_nl_mode(&AtNewline_Continue{}, func(p *Parser) {
		p.assert(SyntaxKindLeftParen)
		if p.eat_if(SyntaxKindColon) {
			state.kind = option.Some(SyntaxKindDict)
		}

		for !p.current().IsTerminator() {
			if !p.at_set(ArrayOrDictItemSet) {
				p.unexpected()
				continue
			}

			array_or_dict_item(p, state)
			state.count += 1

			if !p.current().IsTerminator() && p.expect(SyntaxKindComma) {
				state.maybe_just_parens = false
			}
		}

		p.expect_closing_delimiter(m, SyntaxKindRightParen)
	})

	var kind SyntaxKind
	if state.maybe_just_parens && state.count == 1 {
		kind = SyntaxKindParenthesized
	} else {
		if k, ok := state.kind.Get(); ok {
			kind = k
		} else {
			kind = SyntaxKindArray
		}
	}

	p.wrap(m, kind)
	return kind
}

// State for array/dictionary parsing.
type GroupState struct {
	count uint
	// Whether this is just a single expression in parens: `(a)`. Single
	// element arrays require an explicit comma: `(a,)`, unless we're
	// spreading: `(..a)`.
	maybe_just_parens bool
	// The `SyntaxKind` to wrap as (if we've figured it out yet).
	kind option.Option[SyntaxKind]
	// Store named arguments so we can give an error if they're repeated.
	seen map[string]bool
}

// Parses a single item in an array or dictionary.
func array_or_dict_item(p *Parser, state *GroupState) {
	m := p.marker()

	if p.eat_if(SyntaxKindDots) {
		// Parses a spread item: `..item`.
		code_expr(p)
		p.wrap(m, SyntaxKindSpread)
		state.maybe_just_parens = false
		return
	}

	code_expr(p)

	if p.eat_if(SyntaxKindColon) {
		// Parses a named/keyed pair: `name: item` or `"key": item`.
		code_expr(p)

		node := p.index(m) // NOTE: was index_mut
		var pair_kind SyntaxKind
		switch node.kind() {
		case SyntaxKindIdent:
			pair_kind = SyntaxKindNamed
		default:
			pair_kind = SyntaxKindKeyed
		}

		var key string
		if expr, ok := SyntaxNode_cast[Expr](node).Get(); ok {
			switch expr := expr.(type) {
			case *Ident:
				key = expr.get()
			case *Str:
				key = expr.get()
			}
		}
		if len(key) > 0 {
			if !map_insert(state.seen, key) {
				node.convert_to_error(fmt.Sprintf("duplicate key: %v", key))
			}
		}

		p.wrap(m, pair_kind)
		state.maybe_just_parens = false

		if kind, ok := state.kind.Get(); ok && kind == SyntaxKindArray {
			p.index(m).expected("expression")
		} else {
			state.kind = option.Some(SyntaxKindDict)
		}
	} else {
		// Parses a positional item.
		if kind, ok := state.kind.Get(); ok && kind == SyntaxKindDict {
			p.index(m).expected("named or keyed pair")
		} else {
			state.kind = option.Some(SyntaxKindArray)
		}
	}
}

// Parses a function call's argument list: `(12pt, y)`.
func args(p *Parser) {
	if !p.directly_at(SyntaxKindLeftParen) && !p.directly_at(SyntaxKindLeftBracket) {
		p.expected("argument list")
		if p.at(SyntaxKindLeftParen) || p.at(SyntaxKindLeftBracket) {
			p.hint("there may not be any spaces before the argument list")
		}
	}

	m := p.marker()
	if p.at(SyntaxKindLeftParen) {
		m2 := p.marker()
		p.with_nl_mode(&AtNewline_Continue{}, func(p *Parser) {
			p.assert(SyntaxKindLeftParen)

			seen := make(map[string]bool)
			for !p.current().IsTerminator() {
				if !p.at_set(ArgSet) {
					p.unexpected()
					continue
				}

				arg(p, seen)

				if !p.current().IsTerminator() {
					p.expect(SyntaxKindComma)
				}
			}

			p.expect_closing_delimiter(m2, SyntaxKindRightParen)
		})
	}

	for p.directly_at(SyntaxKindLeftBracket) {
		content_block(p)
	}

	p.wrap(m, SyntaxKindArgs)
}

// Parses a single argument in an argument list.
func arg(p *Parser, seen map[string]bool) {
	m := p.marker()

	// Parses a spread argument: `..args`.
	if p.eat_if(SyntaxKindDots) {
		code_expr(p)
		p.wrap(m, SyntaxKindSpread)
		return
	}

	// Parses a normal positional argument or an argument name.
	was_at_expr := p.at_set(CodeExprSet)
	text := p.current_text()
	code_expr(p)

	// Parses a named argument: `thickness: 12pt`.
	if p.eat_if(SyntaxKindColon) {
		// Recover from bad argument name.
		if was_at_expr {
			if p.index(m).kind() != SyntaxKindIdent {
				p.index(m).expected("identifier")
			} else if !map_insert(seen, text) {
				p.index(m).convert_to_error(fmt.Sprintf("duplicate argument: %v", text))
			}
		}

		code_expr(p)
		p.wrap(m, SyntaxKindNamed)
	}
}

// Parses a closure's parameters: `(x, y)`.
func params(p *Parser) {
	m := p.marker()
	p.with_nl_mode(&AtNewline_Continue{}, func(p *Parser) {
		p.assert(SyntaxKindLeftParen)

		seen := make(map[string]bool)
		sink := false

		for !p.current().IsTerminator() {
			if !p.at_set(ParamSet) {
				p.unexpected()
				continue
			}

			param(p, seen, &sink)

			if !p.current().IsTerminator() {
				p.expect(SyntaxKindComma)
			}
		}

		p.expect_closing_delimiter(m, SyntaxKindRightParen)
	})
	p.wrap(m, SyntaxKindParams)
}

// Parses a single parameter in a parameter list.
func param(p *Parser, seen map[string]bool, sink *bool) {
	m := p.marker()

	// Parses argument sink: `..sink`.
	if p.eat_if(SyntaxKindDots) {
		if p.at_set(PatternLeafSet) {
			pattern_leaf(p, false, seen, option.Some("parameter"))
		}
		p.wrap(m, SyntaxKindSpread)
		prev_sink := *sink
		*sink = true
		if prev_sink {
			p.index(m).convert_to_error("only one argument sink is allowed")
		}
		return
	}

	// Parses a normal positional parameter or a parameter name.
	was_at_pat := p.at_set(PatternSet)
	pattern(p, false, seen, option.Some("parameter"))

	// Parses a named parameter: `thickness: 12pt`.
	if p.eat_if(SyntaxKindColon) {
		// Recover from bad parameter name.
		if was_at_pat && p.index(m).kind() != SyntaxKindIdent {
			p.index(m).expected("identifier")
		}

		code_expr(p)
		p.wrap(m, SyntaxKindNamed)
	}
}

// Parses a binding or reassignment pattern.
func pattern(p *Parser, reassignment bool, seen map[string]bool, dupe option.Option[string]) {
	switch p.current() {
	case SyntaxKindUnderscore:
		p.eat()
	case SyntaxKindLeftParen:
		destructuring_or_parenthesized(p, reassignment, seen)
	default:
		pattern_leaf(p, reassignment, seen, dupe)
	}
}

// Parses a destructuring pattern or just a parenthesized pattern.
func destructuring_or_parenthesized(p *Parser, reassignment bool, seen map[string]bool) {
	sink := false
	count := 0
	maybe_just_parens := true

	m := p.marker()
	p.with_nl_mode(&AtNewline_Continue{}, func(p *Parser) {
		p.assert(SyntaxKindLeftParen)

		for !p.current().IsTerminator() {
			if !p.at_set(DestructuringItemSet) {
				p.unexpected()
				continue
			}

			destructuring_item(p, reassignment, seen, &maybe_just_parens, &sink)
			count += 1

			if !p.current().IsTerminator() && p.expect(SyntaxKindComma) {
				maybe_just_parens = false
			}
		}

		p.expect_closing_delimiter(m, SyntaxKindRightParen)
	})

	if maybe_just_parens && count == 1 && !sink {
		p.wrap(m, SyntaxKindParenthesized)
	} else {
		p.wrap(m, SyntaxKindDestructuring)
	}
}

// Parses an item in a destructuring pattern.
func destructuring_item(p *Parser, reassignment bool, seen map[string]bool, maybe_just_parens, sink *bool) {
	m := p.marker()

	// Parse destructuring sink: `..rest`.
	if p.eat_if(SyntaxKindDots) {
		if p.at_set(PatternLeafSet) {
			pattern_leaf(p, reassignment, seen, option.None[string]())
		}
		p.wrap(m, SyntaxKindSpread)
		prev_sink := *sink
		*sink = true
		if prev_sink {
			p.index(m).convert_to_error("only one destructuring sink is allowed")
		}
		return
	}

	// Parse a normal positional pattern or a destructuring key.
	was_at_pat := p.at_set(PatternSet)

	// We must use a full checkpoint here (can't just clone the lexer) because
	// there may be trivia between the identifier and the colon we need to skip.
	checkpoint := p.checkpoint()
	if !(p.eat_if(SyntaxKindIdent) && p.at(SyntaxKindColon)) {
		p.restore(checkpoint)
		pattern(p, reassignment, seen, option.None[string]())
	}

	// Parse named destructuring item.
	if p.eat_if(SyntaxKindColon) {
		// Recover from bad named destructuring.
		if was_at_pat && p.index(m).kind() != SyntaxKindIdent {
			p.index(m).expected("identifier")
		}

		pattern(p, reassignment, seen, option.None[string]())
		p.wrap(m, SyntaxKindNamed)
		*maybe_just_parens = false
	}
}

// Parses a leaf in a pattern - either an identifier or an expression
// depending on whether it's a binding or reassignment pattern.
func pattern_leaf(p *Parser, reassignment bool, seen map[string]bool, dupe option.Option[string]) {
	if p.current().IsKeyword() {
		p.eat_and_get().expected("pattern")
		return
	} else if !p.at_set(PatternLeafSet) {
		p.expected("pattern")
		return
	}

	m := p.marker()
	text := p.current_text()

	// We parse an atomic expression even though we only want an identifier for
	// better error recovery. We can mark the whole expression as unexpected
	// instead of going through its pieces one by one.
	code_expr_prec(p, true, 0)

	if !reassignment {
		node := p.index(m) // NOTE: was index_mut
		if node.kind() == SyntaxKindIdent {
			if !map_insert(seen, text) {
				_dupe := "binding"
				if d, ok := dupe.Get(); ok {
					_dupe = d
				}
				node.convert_to_error(fmt.Sprintf("duplicate %v: %v", _dupe, text))
			}
		} else {
			node.expected("pattern")
		}
	}
}

// Manages parsing a stream of tokens into a tree of [`SyntaxNode`]s.
//
// The implementation presents an interface that investigates a current `token`
// with a [`SyntaxKind`] and can take one of the following actions:
//
//  1. Eat a token: push `token` onto the `nodes` vector as a [leaf
//     node](`SyntaxNode::leaf`) and prepare a new `token` by calling into the
//     lexer.
//  2. Wrap nodes from a marker to the end of `nodes` (excluding `token` and any
//     attached trivia) into an [inner node](`SyntaxNode::inner`) of a specific
//     `SyntaxKind`.
//  3. Produce or convert nodes into an [error node](`SyntaxNode::error`) when
//     something expected is missing or something unexpected is found.
//
// Overall the parser produces a nested tree of SyntaxNodes as a "_Concrete_
// Syntax Tree." The raw Concrete Syntax Tree should contain the entire source
// text, and is used as-is for e.g. syntax highlighting and IDE features. In
// `ast.rs` the CST is interpreted as a lazy view over an "_Abstract_ Syntax
// Tree." The AST module skips over irrelevant tokens -- whitespace, comments,
// code parens, commas in function args, etc. -- as it iterates through the
// tree.
//
// ### Modes
//
// The parser manages the transitions between the three modes of Typst through
// [syntax modes](`SyntaxMode`) and [newline modes](`AtNewline`).
//
// The syntax modes map to the three Typst modes and are stored in the lexer,
// changing which `SyntaxKind`s it will generate.
//
// The newline mode is used to determine whether a newline should end the
// current expression. If so, the parser temporarily changes `token`'s kind to
// a fake [`SyntaxKindEnd`]. When the parser exits the mode the original
// `SyntaxKind` is restored.
type Parser struct {
	// The source text shared with the lexer.
	text string
	// A lexer over the source text with multiple modes. Defines the boundaries
	// of tokens and determines their [`SyntaxKind`]. Contains the [`SyntaxMode`]
	// defining our current Typst mode.
	lexer *Lexer
	// The newline mode: whether to insert a temporary end at newlines.
	nl_mode AtNewline
	// The current token under inspection, not yet present in `nodes`. This
	// acts like a single item of lookahead for the parser.
	//
	// When wrapping, this is _not_ included in the wrapped nodes.
	token *Token
	// Whether the parser has the expected set of open/close delimiters. This
	// only ever transitions from `true` to `false`.
	balanced bool
	// Nodes representing the concrete syntax tree of previously parsed text.
	// In Code and Math, includes previously parsed trivia, but not `token`.
	nodes vector.Vector[*SyntaxNode]
	// Parser checkpoints for a given text index. Used for efficient parser
	// backtracking similar to packrat parsing. See comments above in
	// [`expr_with_paren`].
	memo *MemoArena
}

// A single token returned from the lexer with a cached [`SyntaxKind`] and a
// record of preceding trivia.
type Token struct {
	// The [`SyntaxKind`] of the current token.
	kind SyntaxKind
	// The [`SyntaxNode`] of the current token, ready to be eaten and pushed
	// onto the end of `nodes`.
	node *SyntaxNode
	// The number of preceding trivia before this token.
	n_trivia uint
	// Whether this token's preceding trivia contained a newline.
	newline option.Option[Newline]
	// The index into `text` of the start of our current token (the end is
	// stored as the lexer's cursor).
	start uint
	// The index into `text` of the end of the previous token.
	prev_end uint
}

func (token *Token) Clone() *Token {
	return &Token{
		kind:     token.kind,
		node:     token.node.clone(),
		n_trivia: token.n_trivia,
		newline:  token.newline.Clone(),
		start:    token.start,
		prev_end: token.prev_end,
	}
}

// Information about newlines in a group of trivia.
type Newline struct {
	// The column of the start of the next token in its line.
	column option.Option[uint]
	// Whether any of our newlines were paragraph breaks.
	parbreak bool
}

// --- [ AtNewline ] -----------------------------------------------------------

// How to proceed with parsing when at a newline.
type AtNewline interface {
	isAtNewline()

	// Whether to stop at a newline or continue based on the current context.
	stop_at(newline Newline, kind SyntaxKind) bool
}

func (*AtNewline_Continue) isAtNewline()           {}
func (*AtNewline_Stop) isAtNewline()               {}
func (*AtNewline_ContextualContinue) isAtNewline() {}
func (*AtNewline_StopParBreak) isAtNewline()       {}
func (*AtNewline_RequireColumn) isAtNewline()      {}

// Continue at newlines.
//
// Continue
type AtNewline_Continue struct{}

// Stop at any newline.
//
// Stop
type AtNewline_Stop struct{}

// Continue only if there is a continuation with `else` or `.` (Code only).
//
// ContextualContinue
type AtNewline_ContextualContinue struct{}

// Stop only at a parbreak, not normal newlines (Markup only).
//
// StopParBreak
type AtNewline_StopParBreak struct{}

// Require that the token's column be greater or equal to a column (Markup
// only). If this is `0`, acts like `Continue`; if this is `usize::MAX`,
// acts like `Stop`.
//
// RequireColumn
type AtNewline_RequireColumn struct {
	min_col uint
}

func NewAtNewline_RequireColumn(min_col uint) *AtNewline_RequireColumn {
	return &AtNewline_RequireColumn{
		min_col: min_col,
	}
}

// Whether to stop at a newline or continue based on the current context.
func (nl *AtNewline_Continue) stop_at(newline Newline, kind SyntaxKind) bool {
	return false
}

// Whether to stop at a newline or continue based on the current context.
func (nl *AtNewline_Stop) stop_at(newline Newline, kind SyntaxKind) bool {
	return true
}

// Whether to stop at a newline or continue based on the current context.
func (nl *AtNewline_ContextualContinue) stop_at(newline Newline, kind SyntaxKind) bool {
	switch kind {
	case SyntaxKindElse, SyntaxKindDot:
		return false
	}
	return true
}

// Whether to stop at a newline or continue based on the current context.
func (nl *AtNewline_StopParBreak) stop_at(newline Newline, kind SyntaxKind) bool {
	return newline.parbreak
}

// Whether to stop at a newline or continue based on the current context.
func (nl *AtNewline_RequireColumn) stop_at(newline Newline, kind SyntaxKind) bool {
	// When the column is `None`, the newline doesn't start a
	// column, and we continue parsing. This may happen on the
	// boundary of syntax modes, since we only report a column in
	// Markup.
	if column, ok := newline.column.Get(); ok {
		return column <= nl.min_col
	}
	return false
}

// --- [ Marker ] --------------------------------------------------------------

// A marker representing a node's position in the parser. Mainly used for
// wrapping, but can also index into the parser to access the node, like
// `p.index(m)`.
type Marker uint

// Index into the parser with markers.
func (p *Parser) index(m Marker) *SyntaxNode {
	return p.nodes[m]
}

// === [ Parser ] ==============================================================

// Creating/Consuming the parser and getting info about the current token.

// Create a new parser starting from the given text offset and syntax mode.
//
// new
func NewParser(text string, offset uint, mode SyntaxMode) *Parser {
	lexer := NewLexer(text, mode)
	lexer.jump(offset)
	nl_mode := &AtNewline_Continue{}
	var nodes vector.Vector[*SyntaxNode]
	token := Parser_lex(&nodes, lexer, nl_mode)
	return &Parser{
		text:     text,
		lexer:    lexer,
		nl_mode:  nl_mode,
		token:    token,
		balanced: true,
		nodes:    nodes,
		memo:     NewMemoArena(),
	}
}

// Consume the parser, yielding the full vector of parsed SyntaxNodes.
func (p *Parser) finish() []*SyntaxNode {
	return p.nodes
}

// Consume the parser, generating a single top-level node.
func (p *Parser) finish_into(kind SyntaxKind) *SyntaxNode {
	if !p.at(SyntaxKindEnd) {
		panic("expected to be at end token")
	}
	return NewInner(kind, p.finish())
}

// Similar to a `peek()` function: returns the `kind` of the next token to
// be eaten.
func (p *Parser) current() SyntaxKind {
	return p.token.kind
}

// Whether the current token is a given [`SyntaxKind`].
func (p *Parser) at(kind SyntaxKind) bool {
	return p.token.kind == kind
}

// Whether the current token is contained in a [`SyntaxSet`].
func (p *Parser) at_set(set *SyntaxSet) bool {
	return set.Contains(p.token.kind)
}

// Whether we're at the end of the token stream.
//
// Note: This might be a fake end due to the newline mode.
func (p *Parser) end() bool {
	return p.at(SyntaxKindEnd)
}

// If we're at the given `kind` with no preceding trivia tokens.
func (p *Parser) directly_at(kind SyntaxKind) bool {
	return p.token.kind == kind && !p.had_trivia()
}

// Whether `token` had any preceding trivia.
func (p *Parser) had_trivia() bool {
	return p.token.n_trivia > 0
}

// Whether `token` had a newline among any of its preceding trivia.
func (p *Parser) had_newline() bool {
	return p.token.newline.IsPresent()
}

// The number of characters until the most recent newline from the start of
// the current token. Uses a cached value from the newline mode if present.
func (p *Parser) current_column() uint {
	if newline, ok := p.token.newline.Get(); ok {
		if column, ok := newline.column.Get(); ok {
			return column
		}
	}
	return p.lexer.column(p.token.start)
}

// The current token's text.
func (p *Parser) current_text() string {
	return p.text[p.token.start:p.current_end()]
}

// The offset into `text` of the current token's start.
func (p *Parser) current_start() uint {
	return p.token.start
}

// The offset into `text` of the current token's end.
func (p *Parser) current_end() uint {
	return p.lexer.cursor()
}

// The offset into `text` of the previous token's end.
func (p *Parser) prev_end() uint {
	return p.token.prev_end
}

// -----------------------------------------------------------------------------

// The main parsing interface for generating tokens and eating/modifying nodes.

// A marker that will point to the current token in the parser once it's
// been eaten.
func (p *Parser) marker() Marker {
	return Marker(len(p.nodes))
}

// A marker that will point to first trivia before this token in the
// parser (or the token itself if no trivia precede it).
func (p *Parser) before_trivia() Marker {
	return Marker(uint(len(p.nodes)) - p.token.n_trivia)
}

// Eat the current node and return a reference for in-place mutation.
func (p *Parser) eat_and_get() *SyntaxNode {
	offset := len(p.nodes)
	p.eat()
	return p.nodes[offset]
}

// Eat the token if at `kind`. Returns `true` if eaten.
//
// Note: In Math and Code, this will ignore trivia in front of the
// `kind`, To forbid skipping trivia, consider using `eat_if_direct`.
func (p *Parser) eat_if(kind SyntaxKind) bool {
	at := p.at(kind)
	if at {
		p.eat()
	}
	return at
}

// Eat the token only if at `kind` with no preceding trivia. Returns `true`
// if eaten.
func (p *Parser) eat_if_direct(kind SyntaxKind) bool {
	at := p.directly_at(kind)
	if at {
		p.eat()
	}
	return at
}

// Assert that we are at the given [`SyntaxKind`] and eat it. This should
// be used when moving between functions that expect to start with a
// specific token.
func (p *Parser) assert(kind SyntaxKind) {
	if p.token.kind != kind {
		panic(fmt.Errorf("expected %v, got %v", kind, p.token.kind))
	}
	p.eat()
}

// Convert the current token's [`SyntaxKind`] and eat it.
func (p *Parser) convert_and_eat(kind SyntaxKind) {
	// Only need to replace the node here.
	p.token.node.convert_to_kind(kind)
	p.eat()
}

// Eat the current token by saving it to the `nodes` vector, then move
// the lexer forward to prepare a new token.
func (p *Parser) eat() {
	p.nodes.Push(p.token.node)
	p.token = Parser_lex(&p.nodes, p.lexer, p.nl_mode)
}

// Detach the parsed trivia nodes from this token (but not newline info) so
// that subsequent wrapping will include the trivia.
func (p *Parser) flush_trivia() {
	p.token.n_trivia = 0
	p.token.prev_end = p.token.start
}

// Wrap the nodes from a marker up to (but excluding) the current token in
// a new [inner node](`SyntaxNode::inner`) of the given kind. This is an
// easy interface for creating nested syntax nodes _after_ having parsed
// their children.
func (p *Parser) wrap(from Marker, kind SyntaxKind) {
	to := uint64(p.before_trivia())
	_from := min(uint64(from), to)
	children := p.nodes.Drain(ranges.NewRange(_from, to))
	p.nodes.Insert(int(_from), NewInner(kind, children))
}

// Parse within the [`SyntaxMode`] for subsequent tokens (does not change the
// current token). This may re-lex the final token on exit.
//
// This function effectively repurposes the call stack as a stack of modes.
func (p *Parser) enter_modes(mode SyntaxMode, stop AtNewline, fn func(*Parser)) {
	previous := p.lexer.mode
	p.lexer.set_mode(mode)
	p.with_nl_mode(stop, fn)
	if mode != previous {
		p.lexer.set_mode(previous)
		p.lexer.jump(p.token.prev_end)
		p.nodes.Truncate(len(p.nodes) - int(p.token.n_trivia))
		p.token = Parser_lex(&p.nodes, p.lexer, p.nl_mode)
	}
}

// Parse within the [`AtNewline`] mode for subsequent tokens (does not
// change the current token). This may re-lex the final token on exit.
//
// This function effectively repurposes the call stack as a stack of modes.
func (p *Parser) with_nl_mode(mode AtNewline, fn func(*Parser)) {
	previous := p.nl_mode
	p.nl_mode = mode
	fn(p)
	p.nl_mode = previous
	if newline, ok := p.token.newline.Get(); ok && mode != previous {
		// Restore our actual token's kind or insert a fake end.
		actual_kind := p.token.node.kind()
		if p.nl_mode.stop_at(newline, actual_kind) {
			p.token.kind = SyntaxKindEnd
		} else {
			p.token.kind = actual_kind
		}
	}
}

// Move the lexer forward and prepare the current token. In Code, this
// might insert a temporary [`SyntaxKindEnd`] based on our newline mode.
//
// This is not a method on `self` because we need a valid token before we
// can initialize the parser.
//
// lex
func Parser_lex(nodes *vector.Vector[*SyntaxNode], lexer *Lexer, nl_mode AtNewline) *Token {
	prev_end := lexer.cursor()
	start := prev_end
	kind, node := lexer.next()
	n_trivia := uint(0)
	had_newline := false
	parbreak := false

	for kind.IsTrivia() {
		if lexer.newline {
			had_newline = true // Newlines are always trivia.
		}
		if kind == SyntaxKindParbreak {
			parbreak = true
		}
		n_trivia++
		nodes.Push(node)
		start = lexer.cursor()
		kind, node = lexer.next()
	}

	newline := option.None[Newline]()
	if had_newline {
		column := option.None[uint]()
		if lexer.mode == SyntaxModeMarkup {
			column = option.Some(lexer.column(start))
		}
		_newline := Newline{
			column:   column,
			parbreak: parbreak,
		}
		if nl_mode.stop_at(_newline, kind) {
			// Insert a temporary `SyntaxKindEnd` to halt the parser.
			// The actual kind will be restored from `node` later.
			kind = SyntaxKindEnd
		}
		newline = option.Some(_newline)
	}

	return &Token{
		kind:     kind,
		node:     node,
		n_trivia: n_trivia,
		newline:  newline,
		start:    start,
		prev_end: prev_end,
	}
}

// --- [ MemoArena ] -----------------------------------------------------------

// Extra parser state for efficiently recovering from mispredicted parses.
//
// This is the same idea as packrat parsing, but we use it only in the limited
// case of parenthesized structures. See [`expr_with_paren`] for more.
type MemoArena struct {
	// A single arena of previously parsed nodes (to reduce allocations).
	// Memoized ranges refer to unique sections of the arena.
	arena vector.Vector[*SyntaxNode]
	// A map from the parser's current position to a range of previously parsed
	// nodes in the arena and a checkpoint of the parser's state. These allow
	// us to reset the parser to avoid parsing the same location again.
	memo_map map[MemoKey]*MemoPair
}

func NewMemoArena() *MemoArena {
	return &MemoArena{
		arena:    nil,
		memo_map: make(map[MemoKey]*MemoPair),
	}
}

// A type alias for the memo key so it doesn't get confused with other usizes.
//
// The memo is keyed by the index into `text` of the current token's start.
type MemoKey uint

type MemoPair struct {
	arena_range ranges.Range
	state       *PartialState
}

func (memo_pair *MemoPair) Clone() *MemoPair {
	return &MemoPair{
		arena_range: memo_pair.arena_range,
		state:       memo_pair.state.Clone(),
	}
}

// A checkpoint of the parser which can fully restore it to a previous state.
type Checkpoint struct {
	node_len uint
	state    *PartialState
}

// State needed to restore the parser's current token and the lexer (but not
// the nodes vector).
type PartialState struct {
	cursor   uint
	lex_mode SyntaxMode
	token    *Token
}

func (state *PartialState) Clone() *PartialState {
	return &PartialState{
		cursor:   state.cursor,
		lex_mode: state.lex_mode,
		token:    state.token.Clone(),
	}
}

// -----------------------------------------------------------------------------

// The Memoization interface.

// Store the already parsed nodes and the parser state into the memo map by
// extending the arena and storing the extended range and a checkpoint.
func (p *Parser) memoize_parsed_nodes(key MemoKey, prev_len uint) {
	checkpoint := p.checkpoint()
	memo_start := len(p.memo.arena)
	p.memo.arena.ExtendFromSlice(p.nodes[prev_len:checkpoint.node_len])
	arena_range := ranges.NewRange(uint64(memo_start), uint64(len(p.memo.arena)))
	memo_pair := &MemoPair{
		arena_range: arena_range,
		state:       checkpoint.state,
	}
	p.memo.memo_map[key] = memo_pair
}

type MemoCheckpoint struct {
	memo_key   MemoKey
	checkpoint *Checkpoint
}

// Try to load a memoized result, return `None` if we did or `Some` (with a
// checkpoint and a key for the memo map) if we didn't.
func (p *Parser) restore_memo_or_checkpoint() option.Option[MemoCheckpoint] {
	// We use the starting index of the current token as our key.
	key := MemoKey(p.current_start())
	memo_pair, ok := p.memo.memo_map[key]
	if !ok {
		return option.Some(MemoCheckpoint{
			memo_key:   key,
			checkpoint: p.checkpoint(),
		})
	}
	new_memo_pair := memo_pair.Clone()
	r := new_memo_pair.arena_range
	p.nodes.ExtendFromSlice(p.memo.arena[r.Start:r.End])
	// It's important that we don't truncate the nodes vector since
	// it may have grown or shrunk (due to other memoization or
	// error reporting) since we made this checkpoint.
	p.restore_partial(new_memo_pair.state)
	return option.None[MemoCheckpoint]()
}

// Restore the parser to the state at a checkpoint.
func (p *Parser) restore(checkpoint *Checkpoint) {
	p.nodes.Truncate(int(checkpoint.node_len))
	p.restore_partial(checkpoint.state)
}

// Restore parts of the checkpoint excluding the nodes vector.
func (p *Parser) restore_partial(state *PartialState) {
	p.lexer.jump(state.cursor)
	p.lexer.set_mode(state.lex_mode)
	p.token = state.token
}

// Save a checkpoint of the parser state.
func (p *Parser) checkpoint() *Checkpoint {
	node_len := uint(len(p.nodes))
	state := &PartialState{
		cursor:   p.lexer.cursor(),
		lex_mode: p.lexer.mode,
		token:    p.token.Clone(),
	}
	return &Checkpoint{
		node_len: node_len,
		state:    state,
	}
}

// -----------------------------------------------------------------------------

// Functions for eating expected or unexpected tokens and generating errors if
// we don't get what we expect.

// Consume the given `kind` or produce an error.
func (p *Parser) expect(kind SyntaxKind) bool {
	at := p.at(kind)
	if at {
		p.eat()
	} else if kind == SyntaxKindIdent && p.token.kind.IsKeyword() {
		p.trim_errors()
		p.eat_and_get().expected(kind.Name())
	} else {
		p.balanced = p.balanced && !kind.IsGrouping()
		p.expected(kind.Name())
	}
	return at
}

// Consume the given closing delimiter or produce an error for the matching
// opening delimiter at `open`.
func (p *Parser) expect_closing_delimiter(open Marker, kind SyntaxKind) {
	if !p.eat_if(kind) {
		p.nodes[open].convert_to_error("unclosed delimiter")
	}
}

// Produce an error that the given `thing` was expected.
func (p *Parser) expected(thing string) {
	if !p.after_error() {
		p.expected_at(p.before_trivia(), thing)
	}
}

// Whether the last non-trivia node is an error.
func (p *Parser) after_error() bool {
	m := p.before_trivia()
	if m == 0 {
		return false
	}
	index := int(m) - 1
	if index >= len(p.nodes) {
		return false
	}
	return p.nodes[index].kind().IsError()
}

// Produce an error that the given `thing` was expected at the position
// of the marker `m`.
func (p *Parser) expected_at(m Marker, thing string) {
	error := NewError(NewSyntaxError(fmt.Sprintf("expected %v", thing)), "")
	p.nodes.Insert(int(m), error)
}

// Add a hint to a trailing error.
func (p *Parser) hint(hint string) {
	m := p.before_trivia()
	index := int(m) - 1
	if index >= len(p.nodes) {
		return
	}
	error := p.nodes[index]
	error.hint(hint)
}

// Consume the next token (if any) and produce an error stating that it was
// unexpected.
func (p *Parser) unexpected() {
	p.trim_errors()
	p.balanced = p.balanced && !p.token.kind.IsGrouping()
	p.eat_and_get().unexpected()
}

// Remove trailing errors with zero length.
func (p *Parser) trim_errors() {
	_end := p.before_trivia()
	end := uint64(_end)
	start := end
	for start > 0 && p.nodes[start-1].kind().IsError() && p.nodes[start-1].is_empty() {
		start--
	}
	p.nodes.Drain(ranges.NewRange(start, end))
}

// ### [ Helper functions ] ####################################################

// all reports whether all characters of s fulfill the given condition.
func all(s string, cond func(r rune) bool) bool {
	for _, r := range s {
		if !cond(r) {
			return false
		}
	}
	return true
}

// any reports whether any characters of s fulfills the given condition.
func any_(s string, cond func(r rune) bool) bool {
	for _, r := range s {
		if cond(r) {
			return true
		}
	}
	return false
}

// Adds a value to the set.

// Returns whether the value was newly inserted. That is:
//
//   - If the set did not previously contain this value, true is returned.
//   - If the set already contained this value, false is returned, and the set is
//     not modified: original value is not replaced, and the value passed as
//     argument is dropped.
func map_insert(m map[string]bool, key string) bool {
	if _, present := m[key]; present {
		return false // already present
	}
	m[key] = true
	return true // value inserted
}

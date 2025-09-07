package syntax

import (
	"fmt"
	"strconv"
	"strings"
	"unicode"
	"unicode/utf8"

	"github.com/clipperhouse/uax29/graphemes"
	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
	"github.com/mewmew/typast/internal/scanner"
	"github.com/mewmew/typast/internal/stack"
	"github.com/mewmew/typast/internal/stdx"
	"github.com/pkg/errors"
	"github.com/smasher164/xid"
)

// --- [ Lexer ] ---------------------------------------------------------------

// An iterator over a source code string which returns tokens.
type Lexer struct {
	// The scanner: contains the underlying string and location as a "cursor".
	s *scanner.Scanner
	// The mode the lexer is in. This determines which kinds of tokens it
	// produces.
	mode SyntaxMode
	// Whether the last token contained a newline.
	newline bool
	// An error for the last token.
	err option.Option[*SyntaxError]
}

// Create a new lexer with the given mode and a prefix to offset column
// calculations.
//
// new
func NewLexer(text string, mode SyntaxMode) *Lexer {
	return &Lexer{
		s:       scanner.New(text),
		mode:    mode,
		newline: false,
		err:     option.None[*SyntaxError](),
	}
}

func (lexer *Lexer) clone() *Lexer {
	return &Lexer{
		s:       lexer.s.Clone(),
		mode:    lexer.mode,
		newline: lexer.newline,
		err:     lexer.err.Clone(),
	}
}

// Change the lexing mode.
func (lexer *Lexer) set_mode(mode SyntaxMode) {
	lexer.mode = mode
}

// The index in the string at which the last token ends and next token
// will start.
func (lexer *Lexer) cursor() uint {
	return lexer.s.Cursor()
}

// Jump to the given index in the string.
func (lexer *Lexer) jump(index uint) {
	lexer.s.Jump(index)
}

// The number of characters until the most recent newline from an index.
func (lexer *Lexer) column(index uint) uint {
	s := lexer.s.Clone() // Make a new temporary scanner (cheap).
	s.Jump(index)
	before := s.Before()
	rs := []rune(before) // TODO: optimize when needed.
	n := uint(0)
	for i := len(rs) - 1; i >= 0; i-- {
		if is_newline(rs[i]) {
			break
		}
		n++
	}
	return n
}

// Construct a full-positioned syntax error.
func (lexer *Lexer) error(message string) SyntaxKind {
	lexer.err = option.Some(NewSyntaxError(message))
	return SyntaxKindError
}

// If the current node is an error, adds a hint.
func (lexer *Lexer) hint(message string) {
	if err, ok := lexer.err.Get(); ok {
		err.Hints = append(err.Hints, message)
	}
}

// Shared methods with all [`SyntaxMode`].

// Return the next token in our text. Returns both the [`SyntaxNode`]
// and the raw [`SyntaxKind`] to make it more ergonomic to check the kind
func (lexer *Lexer) next() (SyntaxKind, *SyntaxNode) {
	if err, ok := lexer.err.Get(); ok {
		panic(err.Message)
	}
	start := lexer.s.Cursor()

	lexer.newline = false
	var kind SyntaxKind
	if c, ok := lexer.s.Eat(); ok {
		switch {
		case is_space(c, lexer.mode):
			kind = lexer.whitespace(start, c)
		case c == '#' && start == 0 && lexer.s.EatIf("!"):
			kind = lexer.shebang()
		case c == '/' && lexer.s.EatIf("/"):
			kind = lexer.line_comment()
		case c == '/' && lexer.s.EatIf("*"):
			kind = lexer.block_comment()
		case c == '*' && lexer.s.EatIf("/"):
			kind = lexer.error("unexpected end of block comment")
			lexer.hint("consider escaping the `*` with a backslash or opening the block comment with `/*`")
		case c == '`' && lexer.mode != SyntaxModeMath:
			return lexer.raw()
		default:
			switch lexer.mode {
			case SyntaxModeMarkup:
				kind = lexer.markup(start, c)
			case SyntaxModeMath:
				k, n := lexer.math(start, c)
				if node, ok := n.Get(); ok {
					return kind, node
				} else {
					kind = k
				}
			case SyntaxModeCode:
				kind = lexer.code(start, c)
			}
		}
	} else {
		kind = SyntaxKindEnd
	}

	text := lexer.s.From(start)
	if err, ok := lexer.err.Get(); ok {
		lexer.err = option.None[*SyntaxError]() // reset error
		node := NewError(err, text)
		return kind, node
	}
	node := NewLeaf(kind, text)
	return kind, node
}

// Eat whitespace characters greedily.
func (lexer *Lexer) whitespace(start uint, c rune) SyntaxKind {
	cond := func(c rune) bool {
		return is_space(c, lexer.mode)
	}
	more := lexer.s.EatWhileFunc(cond)
	var newlines uint
	switch {
	case c == ' ' && len(more) == 0:
		// Optimize eating a single space.
		newlines = 0
	default:
		newlines = count_newlines(lexer.s.From(start))
	}

	if newlines > 0 {
		lexer.newline = true
	}
	if lexer.mode == SyntaxModeMarkup && newlines >= 2 {
		return SyntaxKindParbreak
	}
	return SyntaxKindSpace
}

func (lexer *Lexer) shebang() SyntaxKind {
	lexer.s.EatUntilFunc(is_newline)
	return SyntaxKindShebang
}

func (lexer *Lexer) line_comment() SyntaxKind {
	lexer.s.EatUntilFunc(is_newline)
	return SyntaxKindLineComment
}

func (lexer *Lexer) block_comment() SyntaxKind {
	state := '_'
	depth := 1

	// Find the first `*/` that does not correspond to a nested `/*`.
loop:
	for {
		c, ok := lexer.s.Eat()
		if !ok {
			break
		}

		switch {
		case state == '*' && c == '/':
			depth -= 1
			if depth == 0 {
				break loop
			}
			state = '_'
		case state == '/' && c == '*':
			depth += 1
			state = '_'
		default:
			state = c
		}
	}

	return SyntaxKindBlockComment
}

// === [ Markup ] ==============================================================

// Markup.
func (lexer *Lexer) markup(start uint, c rune) SyntaxKind {
	switch {
	case c == '\\':
		return lexer.backslash()
	case c == 'h' && lexer.s.EatIf("ttp://"):
		return lexer.link()
	case c == 'h' && lexer.s.EatIf("ttps://"):
		return lexer.link()
	case c == '<' && lexer.s.AtFunc(is_id_continue):
		return lexer.label()
	case c == '@' && lexer.s.AtFunc(is_id_continue):
		return lexer.ref_marker()

	case c == '.' && lexer.s.EatIf(".."):
		return SyntaxKindShorthand
	case c == '-' && lexer.s.EatIf("--"):
		return SyntaxKindShorthand
	case c == '-' && lexer.s.EatIf("-"):
		return SyntaxKindShorthand
	case c == '-' && lexer.s.EatIf("?"):
		return SyntaxKindShorthand
	case c == '-' && lexer.s.AtFunc(stdx.IsNumeric):
		return SyntaxKindShorthand
	case c == '*' && !lexer.in_word():
		return SyntaxKindStar
	case c == '_' && !lexer.in_word():
		return SyntaxKindUnderscore

	case c == '#':
		return SyntaxKindHash
	case c == '[':
		return SyntaxKindLeftBracket
	case c == ']':
		return SyntaxKindRightBracket
	case c == '\'':
		return SyntaxKindSmartQuote
	case c == '"':
		return SyntaxKindSmartQuote
	case c == '$':
		return SyntaxKindDollar
	case c == '~':
		return SyntaxKindShorthand
	case c == ':':
		return SyntaxKindColon
	case c == '=':
		lexer.s.EatWhile("=")
		if lexer.space_or_end() {
			return SyntaxKindHeadingMarker
		} else {
			return lexer.text()
		}
	case c == '-' && lexer.space_or_end():
		return SyntaxKindListMarker
	case c == '+' && lexer.space_or_end():
		return SyntaxKindEnumMarker
	case c == '/' && lexer.space_or_end():
		return SyntaxKindTermMarker
	case stdx.IsAsciiDigit(c): // c == '0'..='9':
		return lexer.numbering(start)

	default:
		return lexer.text()
	}
}

func (lexer *Lexer) backslash() SyntaxKind {
	if lexer.s.EatIf("u{") {
		hex := lexer.s.EatWhileFunc(stdx.IsAsciiAlphanumeric)
		if !lexer.s.EatIf("}") {
			return lexer.error("unclosed Unicode escape sequence")
		}

		x, err := strconv.ParseUint(hex, 16, 32)
		if err != nil {
			return lexer.error(fmt.Sprintf("invalid Unicode codepoint: %v", hex))
		}
		c := rune(x)
		if !utf8.ValidRune(c) {
			return lexer.error(fmt.Sprintf("invalid Unicode codepoint: %v", hex))
		}

		return SyntaxKindEscape
	}

	if lexer.s.Done() || lexer.s.AtFunc(stdx.IsWhitespace) {
		return SyntaxKindLinebreak
	} else {
		lexer.s.Eat()
		return SyntaxKindEscape
	}
}

// We parse entire raw segments in the lexer as a convenience to avoid
// going to and from the parser for each raw section. See comments in
// [`Self::blocky_raw`] and [`Self::inline_raw`] for specific details.
func (lexer *Lexer) raw() (SyntaxKind, *SyntaxNode) {
	start := lexer.s.Cursor() - 1

	// Determine number of opening backticks.
	backticks := uint(1)
	for lexer.s.EatIf("`") {
		backticks += 1
	}

	// Special case for ``.
	if backticks == 2 {
		nodes := []*SyntaxNode{
			NewLeaf(SyntaxKindRawDelim, "`"),
			NewLeaf(SyntaxKindRawDelim, "`"),
		}
		return SyntaxKindRaw, NewInner(SyntaxKindRaw, nodes)
	}

	// Find end of raw text.
	found := uint(0)
	for found < backticks {
		c, ok := lexer.s.Eat()
		if !ok {
			msg := NewSyntaxError("unclosed raw text")
			error := NewError(msg, lexer.s.From(start))
			return SyntaxKindError, error
		}
		switch c {
		case '`':
			found += 1
		default:
			found = 0
		}
	}
	end := lexer.s.Cursor()

	var nodes []*SyntaxNode

	// A closure for pushing a node onto our raw vector. Assumes the caller
	// will move the scanner to the next location at each step.
	prev_start := start
	push_raw := func(kind SyntaxKind, s *scanner.Scanner) {
		node := NewLeaf(kind, s.From(prev_start))
		nodes = append(nodes, node)
		prev_start = s.Cursor()
	}

	// Opening delimiter.
	lexer.s.Jump(start + backticks)
	push_raw(SyntaxKindRawDelim, lexer.s)

	if backticks >= 3 {
		lexer.blocky_raw(end-backticks, push_raw)
	} else {
		lexer.inline_raw(end-backticks, push_raw)
	}

	// Closing delimiter.
	lexer.s.Jump(end)
	push_raw(SyntaxKindRawDelim, lexer.s)

	return SyntaxKindRaw, NewInner(SyntaxKindRaw, nodes)
}

// Raw blocks parse a language tag, have smart behavior for trimming
// whitespace in the start/end lines, and trim common leading whitespace
// from all other lines as the "dedent". The exact behavior is described
// below.
//
// ### The initial line:
//   - A valid Typst identifier immediately following the opening delimiter
//     is parsed as the language tag.
//   - We check the rest of the line and if all characters are whitespace,
//     trim it. Otherwise we trim a single leading space if present.
//   - If more trimmed characters follow on future lines, they will be
//     merged into the same trimmed element.
//   - If we didn't trim the entire line, the rest is kept as text.
//
// ### Inner lines:
//   - We determine the "dedent" by iterating over the lines. The dedent is
//     the minimum number of leading whitespace characters (not bytes) before
//     each line that has any non-whitespace characters.
//   - The opening delimiter's line does not contribute to the dedent, but
//     the closing delimiter's line does (even if that line is entirely
//     whitespace up to the delimiter).
//   - We then trim the newline and dedent characters of each line, and add a
//     (potentially empty) text element of all remaining characters.
//
// ### The final line:
//   - If the last line is entirely whitespace, it is trimmed.
//   - Otherwise its text is kept like an inner line. However, if the last
//     non-whitespace character of the final line is a backtick, then one
//     ascii space (if present) is trimmed from the end.
func (lexer *Lexer) blocky_raw(inner_end uint, push_raw func(SyntaxKind, *scanner.Scanner)) {
	// Language tag.
	if lexer.s.EatIfFunc(is_id_start) {
		lexer.s.EatWhileFunc(is_id_continue)
		push_raw(SyntaxKindRawLang, lexer.s)
	}

	// The rest of the function operates on the lines between the backticks.
	lines := split_newlines(lexer.s.To(inner_end))

	// Determine dedent level.
	// TODO: figure out how to handle.
	const dedent = 0 // TODO: remove
	/*
		dedent := lines
			.iter()
			.skip(1)
			.filter(|line| !line.chars().all(stdx.IsWhitespace))
			// The line with the closing ``` is always taken into account
			.chain(lines.last())
			.map(|line| line.chars().take_while(|c| c.is_whitespace()).count())
			.min()
			.unwrap_or(0);
	*/

	// TODO: figure out how to handle.
	/*
		// Trim whitespace from the last line. Will be added as a `RawTrimmed`
		// kind by the check for `lexer.s.cursor() != inner_end` below.
		if lines.last().is_some_and(|last| last.chars().all(stdx.IsWhitespace)) {
			lines.pop();
		} else if let Some(last) = lines.last_mut() {
			// If last line ends in a backtick, try to trim a single space. This
			// check must happen before we add the first line since the last and
			// first lines might be the same.
			if last.trim_end().ends_with('`') {
				*last = last.strip_suffix(' ').unwrap_or(last);
			}
		}

		let mut lines = lines.into_iter();

		// Handle the first line: trim if all whitespace, or trim a single space
		// at the start. Note that the first line does not affect the dedent
		// value.
		if let Some(first_line) = lines.next() {
			if first_line.chars().all(stdx.IsWhitespace) {
				scanner_advance(lexer.s, first_line.len())
				// This is the only spot we advance the scanner, but don't
				// immediately call `push_raw`. But the rest of the function
				// ensures we will always add this text to a `RawTrimmed` later.
				debug_assert!(lexer.s.cursor() != inner_end);
				// A proof by cases follows:
				// # First case: The loop runs
				// If the loop runs, there must be a newline following, so
				// `cursor != inner_end`. And if the loop runs, the first thing
				// it does is add a trimmed element.
				// # Second case: The final if-statement runs.
				// To _not_ reach the loop from here, we must have only one or
				// two lines:
				// 1. If one line, we cannot be here, because the first and last
				//    lines are the same, so this line will have been removed by
				//    the check for the last line being all whitespace.
				// 2. If two lines, the loop will run unless the last is fully
				//    whitespace, but if it is, it will have been popped, then
				//    the final if-statement will run because the text removed
				//    by the last line must include at least a newline, so
				//    `cursor != inner_end` here.
			} else {
				let line_end = lexer.s.cursor() + first_line.len();
				if lexer.s.EatIf(" ") {
					// Trim a single space after the lang tag on the first line.
					push_raw(SyntaxKindRawTrimmed, &lexer.s);
				}
				// We know here that the rest of the line is non-empty.
				lexer.s.jump(line_end);
				push_raw(SyntaxKindText, &lexer.s);
			}
		}
	*/
	// Add lines.
	for _, line := range lines {
		// TODO: figure out how to handle.
		const offset = 0 // TODO: remove
		/*
			offset := line.chars().take(dedent).map(char::len_utf8).sum()
		*/
		eat_newline(lexer.s)
		scanner_advance(lexer.s, offset)
		push_raw(SyntaxKindRawTrimmed, lexer.s)
		scanner_advance(lexer.s, uint(len(line))-offset)
		push_raw(SyntaxKindText, lexer.s)
	}

	// Add final trimmed.
	if lexer.s.Cursor() < inner_end {
		lexer.s.Jump(inner_end)
		push_raw(SyntaxKindRawTrimmed, lexer.s)
	}
}

// Inline raw text is split on lines with non-newlines as `Text` kinds and
// newlines as `RawTrimmed`. Inline raw text does not dedent the text, all
// non-newline whitespace is kept.
func (lexer *Lexer) inline_raw(inner_end uint, push_raw func(SyntaxKind, *scanner.Scanner)) {
	for lexer.s.Cursor() < inner_end {
		if lexer.s.AtFunc(is_newline) {
			push_raw(SyntaxKindText, lexer.s)
			eat_newline(lexer.s)
			push_raw(SyntaxKindRawTrimmed, lexer.s)
			continue
		}
		lexer.s.Eat()
	}
	push_raw(SyntaxKindText, lexer.s)
}

func (lexer *Lexer) link() SyntaxKind {
	link, balanced := link_prefix(lexer.s.After())
	scanner_advance(lexer.s, uint(len(link)))

	if !balanced {
		return lexer.error("automatic links cannot contain unbalanced brackets, use the `link` function instead")
	}

	return SyntaxKindLink
}

func (lexer *Lexer) numbering(start uint) SyntaxKind {
	lexer.s.EatWhileFunc(stdx.IsAsciiDigit)

	read := lexer.s.From(start)
	if lexer.s.EatIf(".") && lexer.space_or_end() {
		if valid_uint(read) {
			return SyntaxKindEnumMarker
		}
	}

	return lexer.text()
}

func (lexer *Lexer) ref_marker() SyntaxKind {
	lexer.s.EatWhileFunc(is_valid_in_label_literal)

	// Don't include the trailing characters likely to be part of text.
	for {
		c, ok := lexer.s.Scout(-1)
		if !ok {
			break
		}
		if c == '.' || c == ':' {
			lexer.s.Uneat()
		} else {
			break
		}
	}

	return SyntaxKindRefMarker
}

func (lexer *Lexer) label() SyntaxKind {
	label := lexer.s.EatWhileFunc(is_valid_in_label_literal)
	if len(label) == 0 {
		return lexer.error("label cannot be empty")
	}

	if !lexer.s.EatIf(">") {
		return lexer.error("unclosed label")
	}

	return SyntaxKindLabel
}

var TABLE = [128]bool{
	' ':    true,
	'\t':   true,
	'\n':   true,
	'\x0b': true,
	'\x0c': true,
	'\r':   true,
	'\\':   true,
	'/':    true,
	'[':    true,
	']':    true,
	'~':    true,
	'-':    true,
	'.':    true,
	'\'':   true,
	'"':    true,
	'*':    true,
	'_':    true,
	':':    true,
	'h':    true,
	'`':    true,
	'$':    true,
	'<':    true,
	'>':    true,
	'@':    true,
	'#':    true,
}

func (lexer *Lexer) text() SyntaxKind {
loop:
	for {
		// TODO: double-check that conversion from Rust to Go is correct.
		lexer.s.EatUntilFunc(func(c rune) bool {
			if int(c) < len(TABLE) && TABLE[c] {
				return true
			}
			return stdx.IsWhitespace(c)
		})

		// Continue with the same text node if the thing would become text
		// anyway.
		s := lexer.s.Clone()
		c, ok := s.Eat()
		if !ok {
			break
		}
		switch {
		case c == ' ' && s.AtFunc(stdx.IsAlphanumeric):
			// do nothing
		case c == '/' && !s.AtAny([]rune{'/', '*'}):
			// do nothing
		case c == '-' && !s.AtAny([]rune{'-', '?'}):
			// do nothing
		case c == '.' && !s.At(".."):
			// do nothing
		case c == 'h' && !s.At("ttp://") && !s.At("ttps://"):
			// do nothing
		case c == '@' && !s.AtFunc(is_valid_in_label_literal):
			// do nothing
		default:
			break loop
		}

		*lexer.s = *s
	}

	return SyntaxKindText
}

func (lexer *Lexer) in_word() bool {
	wordy := func(c rune) bool {
		if stdx.IsAlphanumeric(c) {
			scripts := []*unicode.RangeTable{
				unicode.Han,
				unicode.Hiragana,
				unicode.Katakana,
				unicode.Hangul,
			}
			if unicode.In(c, scripts...) {
				return false
			}
			return true
		}
		return false
	}
	prev, ok := lexer.s.Scout(-2)
	if !ok {
		return false
	}
	next, ok := lexer.s.Peek()
	if !ok {
		return false
	}
	return wordy(prev) && wordy(next)
}

func (lexer *Lexer) space_or_end() bool {
	if lexer.s.Done() {
		return true
	}
	if lexer.s.AtFunc(stdx.IsWhitespace) {
		return true
	}
	if lexer.s.At("//") {
		return true
	}
	if lexer.s.At("/*") {
		return true
	}
	return false
}

// === [ Math ] ================================================================

// Math.
func (lexer *Lexer) math(start uint, c rune) (SyntaxKind, option.Option[*SyntaxNode]) {
	var kind SyntaxKind
	switch {
	case c == '\\':
		kind = lexer.backslash()
	case c == '"':
		kind = lexer.string()

	case c == '-' && lexer.s.EatIf(">>"):
		kind = SyntaxKindMathShorthand
	case c == '-' && lexer.s.EatIf(">"):
		kind = SyntaxKindMathShorthand
	case c == '-' && lexer.s.EatIf("->"):
		kind = SyntaxKindMathShorthand
	case c == ':' && lexer.s.EatIf("="):
		kind = SyntaxKindMathShorthand
	case c == ':' && lexer.s.EatIf(":="):
		kind = SyntaxKindMathShorthand
	case c == '!' && lexer.s.EatIf("="):
		kind = SyntaxKindMathShorthand
	case c == '.' && lexer.s.EatIf(".."):
		kind = SyntaxKindMathShorthand
	case c == '[' && lexer.s.EatIf("|"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("==>"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("-->"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("--"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("-<"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("->"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("<-"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("<<"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("=>"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("=="):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("~~"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("="):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("<"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("-"):
		kind = SyntaxKindMathShorthand
	case c == '<' && lexer.s.EatIf("~"):
		kind = SyntaxKindMathShorthand
	case c == '>' && lexer.s.EatIf("->"):
		kind = SyntaxKindMathShorthand
	case c == '>' && lexer.s.EatIf(">>"):
		kind = SyntaxKindMathShorthand
	case c == '=' && lexer.s.EatIf("=>"):
		kind = SyntaxKindMathShorthand
	case c == '=' && lexer.s.EatIf(">"):
		kind = SyntaxKindMathShorthand
	case c == '=' && lexer.s.EatIf(":"):
		kind = SyntaxKindMathShorthand
	case c == '>' && lexer.s.EatIf("="):
		kind = SyntaxKindMathShorthand
	case c == '>' && lexer.s.EatIf(">"):
		kind = SyntaxKindMathShorthand
	case c == '|' && lexer.s.EatIf("->"):
		kind = SyntaxKindMathShorthand
	case c == '|' && lexer.s.EatIf("=>"):
		kind = SyntaxKindMathShorthand
	case c == '|' && lexer.s.EatIf("]"):
		kind = SyntaxKindMathShorthand
	case c == '|' && lexer.s.EatIf("|"):
		kind = SyntaxKindMathShorthand
	case c == '~' && lexer.s.EatIf("~>"):
		kind = SyntaxKindMathShorthand
	case c == '~' && lexer.s.EatIf(">"):
		kind = SyntaxKindMathShorthand
	case c == '*' || c == '-' || c == '~':
		kind = SyntaxKindMathShorthand

	case c == '.':
		kind = SyntaxKindDot
	case c == ',':
		kind = SyntaxKindComma
	case c == ';':
		kind = SyntaxKindSemicolon
	case c == ')':
		kind = SyntaxKindRightParen

	case c == '#':
		kind = SyntaxKindHash
	case c == '_':
		kind = SyntaxKindUnderscore
	case c == '$':
		kind = SyntaxKindDollar
	case c == '/':
		kind = SyntaxKindSlash
	case c == '^':
		kind = SyntaxKindHat
	case c == '\'':
		kind = SyntaxKindPrime
	case c == '&':
		kind = SyntaxKindMathAlignPoint
	case c == '√' || c == '∛' || c == '∜':
		kind = SyntaxKindRoot

	// Identifiers.
	case is_math_id_start(c) && lexer.s.AtFunc(is_math_id_continue):
		lexer.s.EatWhileFunc(is_math_id_continue)
		kind, node := lexer.math_ident_or_field(start)
		return kind, option.Some(node)

	// Other math atoms.
	default:
		kind = lexer.math_text(start, c)
	}
	return kind, option.None[*SyntaxNode]()
}

// Parse a single `MathIdent` or an entire `FieldAccess`.
func (lexer *Lexer) math_ident_or_field(start uint) (SyntaxKind, *SyntaxNode) {
	kind := SyntaxKindMathIdent
	node := NewLeaf(kind, lexer.s.From(start))
	for {
		ident, ok := lexer.maybe_dot_ident()
		if !ok {
			break
		}
		kind = SyntaxKindFieldAccess
		field_children := []*SyntaxNode{
			node,
			NewLeaf(SyntaxKindDot, "."),
			NewLeaf(SyntaxKindIdent, ident),
		}
		node = NewInner(kind, field_children)
	}
	return kind, node
}

// If at a dot and a math identifier, eat and return the identifier.
func (lexer *Lexer) maybe_dot_ident() (string, bool) {
	c, ok := lexer.s.Scout(1)
	if !ok {
		return "", false
	}
	if is_math_id_start(c) && lexer.s.EatIf(".") {
		ident_start := lexer.s.Cursor()
		lexer.s.Eat()
		lexer.s.EatWhileFunc(is_math_id_continue)
		return lexer.s.From(ident_start), true
	}
	return "", false
}

func (lexer *Lexer) math_text(start uint, c rune) SyntaxKind {
	// Keep numbers and grapheme clusters together.
	if stdx.IsNumeric(c) {
		lexer.s.EatWhileFunc(stdx.IsNumeric)
		s := lexer.s.Clone()
		if s.EatIf(".") && len(s.EatWhileFunc(stdx.IsNumeric)) > 0 {
			*lexer.s = *s
		}
		return SyntaxKindMathText
	} else {
		text := lexer.s.Get(ranges.NewRange(uint64(start), uint64(len(lexer.s.Text()))))
		length, ok := GraphemeLen(text)
		if !ok {
			length = 0
		}
		lexer.s.Jump(start + uint(length))
		if length > utf8.RuneLen(c) {
			// Grapheme clusters are treated as normal text and stay grouped
			// This may need to change in the future.
			return SyntaxKindText
		} else {
			return SyntaxKindMathText
		}
	}
}

// Handle named arguments in math function call.
func (lexer *Lexer) maybe_math_named_arg(start uint) option.Option[*SyntaxNode] {
	cursor := lexer.s.Cursor()
	lexer.s.Jump(start)
	if lexer.s.EatIfFunc(is_id_start) {
		lexer.s.EatWhileFunc(is_id_continue)
		// Check that a colon directly follows the identifier, and not the
		// `:=` or `::=` math shorthands.
		if lexer.s.At(":") && !lexer.s.At(":=") && !lexer.s.At("::=") {
			// Check that the identifier is not just `_`.
			if lexer.s.From(start) == "_" {
				msg := NewSyntaxError("expected identifier, found underscore")
				node := NewError(msg, lexer.s.From(start))
				return option.Some(node)
			}
			node := NewLeaf(SyntaxKindIdent, lexer.s.From(start))
			return option.Some(node)
		}
	}
	lexer.s.Jump(cursor)
	return option.None[*SyntaxNode]()
}

// Handle spread arguments in math function call.
func (lexer *Lexer) maybe_math_spread_arg(start uint) option.Option[*SyntaxNode] {
	cursor := lexer.s.Cursor()
	lexer.s.Jump(start)
	if lexer.s.EatIf("..") {
		// Check that neither a space nor a dot follows the spread syntax.
		// A dot would clash with the `...` math shorthand.
		if !lexer.space_or_end() && !lexer.s.At(".") {
			node := NewLeaf(SyntaxKindDots, lexer.s.From(start))
			return option.Some(node)
		}
	}
	lexer.s.Jump(cursor)
	return option.None[*SyntaxNode]()
}

// === [ Code ] ================================================================

// Code.
func (lexer *Lexer) code(start uint, c rune) SyntaxKind {
	switch {
	case c == '<' && lexer.s.AtFunc(is_id_continue):
		return lexer.label()
	case stdx.IsAsciiDigit(c): // c == '0'..='9':
		return lexer.number(start, c)
	case c == '.' && lexer.s.AtFunc(stdx.IsAsciiDigit):
		return lexer.number(start, c)
	case c == '"':
		return lexer.string()

	case c == '=' && lexer.s.EatIf("="):
		return SyntaxKindEqEq
	case c == '!' && lexer.s.EatIf("="):
		return SyntaxKindExclEq
	case c == '<' && lexer.s.EatIf("="):
		return SyntaxKindLtEq
	case c == '>' && lexer.s.EatIf("="):
		return SyntaxKindGtEq
	case c == '+' && lexer.s.EatIf("="):
		return SyntaxKindPlusEq
	case c == '-' || c == '\u2212' && lexer.s.EatIf("="):
		return SyntaxKindHyphEq
	case c == '*' && lexer.s.EatIf("="):
		return SyntaxKindStarEq
	case c == '/' && lexer.s.EatIf("="):
		return SyntaxKindSlashEq
	case c == '.' && lexer.s.EatIf("."):
		return SyntaxKindDots
	case c == '=' && lexer.s.EatIf(">"):
		return SyntaxKindArrow

	case c == '{':
		return SyntaxKindLeftBrace
	case c == '}':
		return SyntaxKindRightBrace
	case c == '[':
		return SyntaxKindLeftBracket
	case c == ']':
		return SyntaxKindRightBracket
	case c == '(':
		return SyntaxKindLeftParen
	case c == ')':
		return SyntaxKindRightParen
	case c == '$':
		return SyntaxKindDollar
	case c == ',':
		return SyntaxKindComma
	case c == ';':
		return SyntaxKindSemicolon
	case c == ':':
		return SyntaxKindColon
	case c == '.':
		return SyntaxKindDot
	case c == '+':
		return SyntaxKindPlus
	case c == '-' || c == '\u2212':
		return SyntaxKindMinus
	case c == '*':
		return SyntaxKindStar
	case c == '/':
		return SyntaxKindSlash
	case c == '=':
		return SyntaxKindEq
	case c == '<':
		return SyntaxKindLt
	case c == '>':
		return SyntaxKindGt

	case is_id_start(c):
		return lexer.ident(start)

	default:
		return lexer.error(fmt.Sprintf("the character `%v` is not valid in code", c))
	}
}

func (lexer *Lexer) ident(start uint) SyntaxKind {
	lexer.s.EatWhileFunc(is_id_continue)
	ident := lexer.s.From(start)

	prev := lexer.s.Get(ranges.NewRange(0, uint64(start)))
	if (!strings.HasSuffix(prev, ".") && !strings.HasSuffix(prev, "@")) || strings.HasSuffix(prev, "..") {
		if kw, ok := keyword(ident).Get(); ok {
			return kw
		}
	}

	if ident == "_" {
		return SyntaxKindUnderscore
	}
	return SyntaxKindIdent
}

func (lexer *Lexer) number(start uint, first_c rune) SyntaxKind {
	// Handle alternative integer bases.
	var base int
	switch {
	case first_c == '0' && lexer.s.EatIf("b"):
		base = 2
	case first_c == '0' && lexer.s.EatIf("o"):
		base = 8
	case first_c == '0' && lexer.s.EatIf("x"):
		base = 16
	default:
		base = 10
	}

	// Read the initial digits.
	if base == 16 {
		lexer.s.EatWhileFunc(stdx.IsAsciiAlphanumeric)
	} else {
		lexer.s.EatWhileFunc(stdx.IsAsciiDigit)
	}

	// Read floating point digits and exponents.
	is_float := false
	if base == 10 {
		// Read digits following a dot. Make sure not to confuse a spread
		// operator or a method call for the decimal separator.
		if first_c == '.' {
			is_float = true // We already ate the trailing digits above.
		} else if !lexer.s.At("..") {
			if c, ok := lexer.s.Scout(1); ok {
				if !is_id_start(c) {
					if lexer.s.EatIf(".") {
						is_float = true
						lexer.s.EatWhileFunc(stdx.IsAsciiDigit)
					}
				}
			}
		}

		// Read the exponent.
		if !lexer.s.At("em") && lexer.s.EatIfAny([]rune{'e', 'E'}) {
			is_float = true
			lexer.s.EatIfAny([]rune{'+', '-'})
			lexer.s.EatWhileFunc(stdx.IsAsciiDigit)
		}
	}

	number := lexer.s.From(start)
	suffix := lexer.s.EatWhileFunc(func(c rune) bool {
		return stdx.IsAsciiAlphanumeric(c) || c == '%'
	})

	var (
		suffix_err error
		has_suffix bool
	)
	switch suffix {
	case "":
		has_suffix = false
	case "pt", "mm", "cm", "in", "deg", "rad", "em", "fr", "%":
		has_suffix = true
	default:
		suffix_err = fmt.Errorf("invalid number suffix: %v", suffix)
	}

	var number_err error
	if is_float && !valid_float(number) {
		// The only invalid case should be when a float lacks digits after
		// the exponent: e.g. `1.2e`, `2.3E-`, or `1EM`.
		number_err = fmt.Errorf("invalid floating point number: %v", number)
	} else if base == 10 {
		// valid number
	} else {
		base_name := map[int]string{
			2:  "binary",
			8:  "octal",
			16: "hexadecimal",
		}
		name := base_name[base]
		// The index `[2..]` skips the leading `0b`/`0o`/`0x`.
		_number := number[2:]
		value, err := strconv.ParseInt(_number, base, 64)
		if err != nil {
			number_err = errors.Errorf("invalid %v number: %v", name, number)
		} else {
			if len(suffix) == 0 {
				// valid number
			} else {
				if suffix_err == nil {
					suffix_err = errors.Errorf("try using a decimal number: %v%v", value, suffix)
				}
				number_err = errors.Errorf("%v numbers cannot have a suffix", name)
			}
		}
	}

	// Return our number or write an error with helpful hints.
	switch {
	// Valid numbers :D
	case number_err == nil && suffix_err == nil && !has_suffix && is_float:
		return SyntaxKindFloat
	case number_err == nil && suffix_err == nil && !has_suffix:
		return SyntaxKindInt
	case number_err == nil && suffix_err == nil && has_suffix:
		return SyntaxKindNumeric
	// Invalid numbers :(
	case number_err != nil && suffix_err != nil:
		err := lexer.error(number_err.Error())
		lexer.hint(suffix_err.Error())
		return err
	case number_err != nil && suffix_err == nil:
		return lexer.error(number_err.Error())
	case number_err == nil && suffix_err != nil:
		return lexer.error(suffix_err.Error())
	default:
		panic("unreachable")
	}
}

func (lexer *Lexer) string() SyntaxKind {
	escaped := false
	lexer.s.EatUntilFunc(func(c rune) bool {
		stop := c == '"' && !escaped
		escaped = c == '\\' && !escaped
		return stop
	})

	if !lexer.s.EatIf("\"") {
		return lexer.error("unclosed string")
	}

	return SyntaxKindStr
}

// -----------------------------------------------------------------------------

// Try to parse an identifier into a keyword.
func keyword(ident string) option.Option[SyntaxKind] {
	switch ident {
	case "none":
		return option.Some(SyntaxKindNone)
	case "auto":
		return option.Some(SyntaxKindAuto)
	case "true":
		return option.Some(SyntaxKindBool)
	case "false":
		return option.Some(SyntaxKindBool)
	case "not":
		return option.Some(SyntaxKindNot)
	case "and":
		return option.Some(SyntaxKindAnd)
	case "or":
		return option.Some(SyntaxKindOr)
	case "let":
		return option.Some(SyntaxKindLet)
	case "set":
		return option.Some(SyntaxKindSet)
	case "show":
		return option.Some(SyntaxKindShow)
	case "context":
		return option.Some(SyntaxKindContext)
	case "if":
		return option.Some(SyntaxKindIf)
	case "else":
		return option.Some(SyntaxKindElse)
	case "for":
		return option.Some(SyntaxKindFor)
	case "in":
		return option.Some(SyntaxKindIn)
	case "while":
		return option.Some(SyntaxKindWhile)
	case "break":
		return option.Some(SyntaxKindBreak)
	case "continue":
		return option.Some(SyntaxKindContinue)
	case "return":
		return option.Some(SyntaxKindReturn)
	case "import":
		return option.Some(SyntaxKindImport)
	case "include":
		return option.Some(SyntaxKindInclude)
	case "as":
		return option.Some(SyntaxKindAs)
	default:
		return option.None[SyntaxKind]()
	}
}

// --- [ Scanner ] -------------------------------------------------------------

// advance
func scanner_advance(s *scanner.Scanner, by uint) {
	s.Jump(s.Cursor() + by)
}

func eat_newline(s *scanner.Scanner) bool {
	ate := s.EatIfFunc(is_newline)
	if ate && strings.HasSuffix(s.Before(), "\r") {
		s.EatIf("\n")
	}
	return ate
}

// --- [/ Scanner ] ------------------------------------------------------------

// Whether a character will become a [`SyntaxKindSpace`] token.
func is_space(c rune, mode SyntaxMode) bool {
	switch mode {
	case SyntaxModeMarkup:
		return c == ' ' || c == '\t' || is_newline(c)
	default:
		return stdx.IsWhitespace(c)
	}
}

// Whether a character is interpreted as a newline by Typst.
func is_newline(c rune) bool {
	switch c {
	case '\n':
		return true // Line Feed
	case '\x0B':
		return true // Vertical Tab
	case '\x0C':
		return true // Form Feed
	case '\r':
		return true // Carriage Return
	case '\u0085':
		return true // Next Line
	case '\u2028':
		return true // Line Separator
	case '\u2029':
		return true // Paragraph Separator
	}
	return false
}

// Extracts a prefix of the text that is a link and also returns whether the
// parentheses and brackets in the link were balanced.
func link_prefix(text string) (string, bool) {
	s := scanner.New(text)
	brackets := stack.New[rune]()

	s.EatWhileFunc(func(c rune) bool {
		switch {
		case stdx.IsAsciiDigit(c): // '0' ..= '9'
			return true
		case stdx.IsAsciiAlphabetic(c): // 'a' ..= 'z', 'A' ..= 'Z'
			return true
		}
		switch c {
		case '!', '#', '$', '%', '&', '*', '+', ',', '-', '.', '/', ':', ';', '=', '?', '@', '_', '~', '\'':
			return true
		case '[':
			brackets.Push('[')
			return true
		case '(':
			brackets.Push('(')
			return true
		case ']':
			c, ok := brackets.Pop()
			if !ok {
				return false
			}
			return c == '['
		case ')':
			c, ok := brackets.Pop()
			if !ok {
				return false
			}
			return c == '('
		default:
			return false
		}
	})

	// Don't include the trailing characters likely to be part of text.
loop:
	for {
		c, ok := s.Scout(-1)
		if !ok {
			break
		}
		switch c {
		case '!', ',', '.', ':', ';', '?', '\'':
			s.Uneat()
		default:
			break loop
		}
	}

	return s.Before(), brackets.IsEmpty()
}

// Split text at newlines. These newline characters are not kept.
func split_newlines(text string) []string {
	s := scanner.New(text)
	var lines []string
	start := uint(0)
	end := uint(0)

	for {
		c, ok := s.Eat()
		if !ok {
			break
		}
		if is_newline(c) {
			if c == '\r' {
				s.EatIf("\n")
			}

			line := text[start:end]
			lines = append(lines, line)
			start = s.Cursor()
		}
		end = s.Cursor()
	}

	line := text[start:]
	lines = append(lines, line)
	return lines
}

// Count the number of newlines in text.
func count_newlines(text string) uint {
	newlines := uint(0)
	s := scanner.New(text)
	for {
		c, ok := s.Eat()
		if !ok {
			break
		}
		if is_newline(c) {
			if c == '\r' {
				s.EatIf("\n")
			}
			newlines++
		}
	}
	return newlines
}

// Whether a string is a valid Typst identifier.
//
// In addition to what is specified in the [Unicode Standard][uax31], we allow:
// - `_` as a starting character,
// - `_` and `-` as continuing characters.
//
// [uax31]: http://www.unicode.org/reports/tr31/
func isIdent(str string) bool {
	if len(str) == 0 {
		return false
	}
	for i, r := range str {
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

// Whether a character can start an identifier.
func is_id_start(c rune) bool {
	return is_xid_start(c) || c == '_'
}

// Whether a character can continue an identifier.
func is_id_continue(c rune) bool {
	return is_xid_continue(c) || c == '_' || c == '-'
}

// Whether a character can start an identifier in math.
func is_math_id_start(c rune) bool {
	return is_xid_start(c)
}

// Whether a character can continue an identifier in math.
func is_math_id_continue(c rune) bool {
	return is_xid_continue(c) && c != '_'
}

// Whether a character can be part of a label literal's name.
func is_valid_in_label_literal(c rune) bool {
	return is_id_continue(c) || c == ':' || c == '.'
}

// Returns true if this string is valid in a label literal.
func is_valid_label_literal_id(id string) bool {
	if len(id) == 0 {
		return false
	}
	for _, c := range id {
		if !is_valid_in_label_literal(c) {
			return false
		}
	}
	return true
}

func is_xid_start(r rune) bool {
	return xid.Start(r)
}

func is_xid_continue(r rune) bool {
	return xid.Continue(r)
}

// graphemes
func GraphemeLen(str string) (int, bool) {
	s := graphemes.NewSegmenter([]byte(str))
	for s.Next() {
		return len(s.Bytes()), true
	}
	return 0, false
}

func valid_float(str string) bool {
	_, err := strconv.ParseFloat(str, 64)
	return err == nil
}

func valid_uint(str string) bool {
	_, err := strconv.ParseUint(str, 10, 64)
	return err == nil
}

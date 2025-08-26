package syntax

// A syntactical building block of a Typst file.
//
// Can be created by the lexer or by the parser.
type SyntaxKind uint8

const (
	// The end of token stream.
	End SyntaxKind = iota + 1
	// An invalid sequence of characters.
	Error

	// A shebang: `#! ...`
	Shebang
	// A line comment: `// ...`.
	LineComment
	// A block comment: `/* ... */`.
	BlockComment

	// The contents of a file or content block.
	Markup
	// Plain text without markup.
	Text
	// Whitespace. Contains at most one newline in markup, as more indicate a
	// paragraph break.
	Space
	// A forced line break: `\`.
	Linebreak
	// A paragraph break, indicated by one or multiple blank lines.
	Parbreak
	// An escape sequence: `\#`, `\u{1F5FA}`.
	Escape
	// A shorthand for a unicode codepoint. For example, `~` for non-breaking
	// space or `-?` for a soft hyphen.
	Shorthand
	// A smart quote: `'` or `"`.
	SmartQuote
	// Strong content: `*Strong*`.
	Strong
	// Emphasized content: `_Emphasized_`.
	Emph
	// Raw text with optional syntax highlighting: `` `...` ``.
	Raw
	// A language tag at the start of raw text: ``typ ``.
	RawLang
	// A raw delimiter consisting of 1 or 3+ backticks: `` ` ``.
	RawDelim
	// A sequence of whitespace to ignore in a raw text: `    `.
	RawTrimmed
	// A hyperlink: `https://typst.org`.
	Link
	// A label: `<intro>`.
	Label
	// A reference: `@target`, `@target[..]`.
	Ref
	// Introduces a reference: `@target`.
	RefMarker
	// A section heading: `= Introduction`.
	Heading
	// Introduces a section heading: `=`, `==`, ...
	HeadingMarker
	// An item in a bullet list: `- ...`.
	ListItem
	// Introduces a list item: `-`.
	ListMarker
	// An item in an enumeration (numbered list): `+ ...` or `1. ...`.
	EnumItem
	// Introduces an enumeration item: `+`, `1.`.
	EnumMarker
	// An item in a term list: `/ Term: Details`.
	TermItem
	// Introduces a term item: `/`.
	TermMarker
	// A mathematical equation: `$x$`, `$ x^2 $`.
	Equation

	// The contents of a mathematical equation: `x^2 + 1`.
	Math
	// A lone text fragment in math: `x`, `25`, `3.1415`, `=`, `|`, `[`.
	MathText
	// An identifier in math: `pi`.
	MathIdent
	// A shorthand for a unicode codepoint in math: `a <= b`.
	MathShorthand
	// An alignment point in math: `&`.
	MathAlignPoint
	// Matched delimiters in math: `[x + y]`.
	MathDelimited
	// A base with optional attachments in math: `a_1^2`.
	MathAttach
	// Grouped primes in math: `a'''`.
	MathPrimes
	// A fraction in math: `x/2`.
	MathFrac
	// A root in math: `√x`, `∛x` or `∜x`.
	MathRoot

	// A hash that switches into code mode: `#`.
	Hash
	// A left curly brace, starting a code block: `{`.
	LeftBrace
	// A right curly brace, terminating a code block: `}`.
	RightBrace
	// A left square bracket, starting a content block: `[`.
	LeftBracket
	// A right square bracket, terminating a content block: `]`.
	RightBracket
	// A left round parenthesis, starting a grouped expression, collection
	// argument or parameter list: `(`.
	LeftParen
	// A right round parenthesis, terminating a grouped expression, collection
	// argument or parameter list: `)`.
	RightParen
	// A comma separator in a sequence: `,`.
	Comma
	// A semicolon terminating an expression: `;`.
	Semicolon
	// A colon between name/key and value in a dictionary, argument or
	// parameter list, or between the term and body of a term list term: `:`.
	Colon
	// The strong text toggle, multiplication operator, and wildcard import
	// symbol: `*`.
	Star
	// Toggles emphasized text and indicates a subscript in math: `_`.
	Underscore
	// Starts and ends a mathematical equation: `$`.
	Dollar
	// The unary plus and binary addition operator: `+`.
	Plus
	// The unary negation and binary subtraction operator: `-`.
	Minus
	// The division operator and fraction operator in math: `/`.
	Slash
	// The superscript operator in math: `^`.
	Hat
	// The prime in math: `'`.
	Prime
	// The field access and method call operator: `.`.
	Dot
	// The assignment operator: `=`.
	Eq
	// The equality operator: `==`.
	EqEq
	// The inequality operator: `!=`.
	ExclEq
	// The less-than operator: `<`.
	Lt
	// The less-than or equal operator: `<=`.
	LtEq
	// The greater-than operator: `>`.
	Gt
	// The greater-than or equal operator: `>=`.
	GtEq
	// The add-assign operator: `+=`.
	PlusEq
	// The subtract-assign operator: `-=`.
	HyphEq
	// The multiply-assign operator: `*=`.
	StarEq
	// The divide-assign operator: `/=`.
	SlashEq
	// Indicates a spread or sink: `..`.
	Dots
	// An arrow between a closure's parameters and body: `=>`.
	Arrow
	// A root: `√`, `∛` or `∜`.
	Root

	// The `not` operator.
	Not
	// The `and` operator.
	And
	// The `or` operator.
	Or
	// The `none` literal.
	None
	// The `auto` literal.
	Auto
	// The `let` keyword.
	Let
	// The `set` keyword.
	Set
	// The `show` keyword.
	Show
	// The `context` keyword.
	Context
	// The `if` keyword.
	If
	// The `else` keyword.
	Else
	// The `for` keyword.
	For
	// The `in` keyword.
	In
	// The `while` keyword.
	While
	// The `break` keyword.
	Break
	// The `continue` keyword.
	Continue
	// The `return` keyword.
	Return
	// The `import` keyword.
	Import
	// The `include` keyword.
	Include
	// The `as` keyword.
	As

	// The contents of a code block.
	Code
	// An identifier: `it`.
	Ident
	// A boolean: `true`, `false`.
	Bool
	// An integer: `120`.
	Int
	// A floating-point number: `1.2`, `10e-4`.
	Float
	// A numeric value with a unit: `12pt`, `3cm`, `2em`, `90deg`, `50%`.
	Numeric
	// A quoted string: `"..."`.
	Str
	// A code block: `{ let x = 1; x + 2 }`.
	CodeBlock
	// A content block: `[*Hi* there!]`.
	ContentBlock
	// A grouped expression: `(1 + 2)`.
	Parenthesized
	// An array: `(1, "hi", 12cm)`.
	Array
	// A dictionary: `(thickness: 3pt, dash: "solid")`.
	Dict
	// A named pair: `thickness: 3pt`.
	Named
	// A keyed pair: `"spacy key": true`.
	Keyed
	// A unary operation: `-x`.
	Unary
	// A binary operation: `a + b`.
	Binary
	// A field access: `properties.age`.
	FieldAccess
	// An invocation of a function or method: `f(x, y)`.
	FuncCall
	// A function call's argument list: `(12pt, y)`.
	Args
	// Spread arguments or an argument sink: `..x`.
	Spread
	// A closure: `(x, y) => z`.
	Closure
	// A closure's parameters: `(x, y)`.
	Params
	// A let binding: `let x = 1`.
	LetBinding
	// A set rule: `set text(...)`.
	SetRule
	// A show rule: `show heading: it => emph(it.body)`.
	ShowRule
	// A contextual expression: `context text.lang`.
	Contextual
	// An if-else conditional: `if x { y } else { z }`.
	Conditional
	// A while loop: `while x { y }`.
	WhileLoop
	// A for loop: `for x in y { z }`.
	ForLoop
	// A module import: `import "utils.typ": a, b, c`.
	ModuleImport
	// Items to import from a module: `a, b, c`.
	ImportItems
	// A path to an imported name from a submodule: `a.b.c`.
	ImportItemPath
	// A renamed import item: `a as d`.
	RenamedImportItem
	// A module include: `include "chapter1.typ"`.
	ModuleInclude
	// A break from a loop: `break`.
	LoopBreak
	// A continue in a loop: `continue`.
	LoopContinue
	// A return from a function: `return`, `return x + 1`.
	FuncReturn
	// A destructuring pattern: `(x, _, ..y)`.
	Destructuring
	// A destructuring assignment expression: `(x, y) = (1, 2)`.
	DestructAssignment
)

var _is_grouping = map[SyntaxKind]bool{
	LeftBracket:  true,
	LeftBrace:    true,
	LeftParen:    true,
	RightBracket: true,
	RightBrace:   true,
	RightParen:   true,
}

// Is this a bracket, brace, or parenthesis?
func (kind SyntaxKind) is_grouping() bool {
	return _is_grouping[kind]
}

var _is_terminator = map[SyntaxKind]bool{
	End:          true,
	Semicolon:    true,
	RightBrace:   true,
	RightParen:   true,
	RightBracket: true,
}

// Does this node terminate a preceding expression?
func (kind SyntaxKind) is_terminator() bool {
	return _is_terminator[kind]
}

var _is_block = map[SyntaxKind]bool{
	CodeBlock:    true,
	ContentBlock: true,
}

// Is this a code or content block.
func (kind SyntaxKind) is_block() bool {
	return _is_block[kind]
}

var _is_stmt = map[SyntaxKind]bool{
	LetBinding:    true,
	SetRule:       true,
	ShowRule:      true,
	ModuleImport:  true,
	ModuleInclude: true,
}

// Does this node need termination through a semicolon or linebreak?
func (kind SyntaxKind) is_stmt() bool {
	return _is_stmt[kind]
}

var _is_keyword = map[SyntaxKind]bool{
	Not:      true,
	And:      true,
	Or:       true,
	None:     true,
	Auto:     true,
	Let:      true,
	Set:      true,
	Show:     true,
	Context:  true,
	If:       true,
	Else:     true,
	For:      true,
	In:       true,
	While:    true,
	Break:    true,
	Continue: true,
	Return:   true,
	Import:   true,
	Include:  true,
	As:       true,
}

// Is this node is a keyword.
func (kind SyntaxKind) is_keyword() bool {
	return _is_keyword[kind]
}

var _is_trivia = map[SyntaxKind]bool{
	Shebang:      true,
	LineComment:  true,
	BlockComment: true,
	Space:        true,
	Parbreak:     true,
}

// Whether this kind of node is automatically skipped by the parser in
// code and math mode.
func (kind SyntaxKind) is_trivia() bool {
	return _is_trivia[kind]
}

var _is_error = map[SyntaxKind]bool{
	Error: true,
}

// Whether this is an error.
func (kind SyntaxKind) is_error() bool {
	return _is_error[kind]
}

var _name = map[SyntaxKind]string{
	End:                "end of tokens",
	Error:              "syntax error",
	Shebang:            "shebang",
	LineComment:        "line comment",
	BlockComment:       "block comment",
	Markup:             "markup",
	Text:               "text",
	Space:              "space",
	Linebreak:          "line break",
	Parbreak:           "paragraph break",
	Escape:             "escape sequence",
	Shorthand:          "shorthand",
	SmartQuote:         "smart quote",
	Strong:             "strong content",
	Emph:               "emphasized content",
	Raw:                "raw block",
	RawLang:            "raw language tag",
	RawTrimmed:         "raw trimmed",
	RawDelim:           "raw delimiter",
	Link:               "link",
	Label:              "label",
	Ref:                "reference",
	RefMarker:          "reference marker",
	Heading:            "heading",
	HeadingMarker:      "heading marker",
	ListItem:           "list item",
	ListMarker:         "list marker",
	EnumItem:           "enum item",
	EnumMarker:         "enum marker",
	TermItem:           "term list item",
	TermMarker:         "term marker",
	Equation:           "equation",
	Math:               "math",
	MathText:           "math text",
	MathIdent:          "math identifier",
	MathShorthand:      "math shorthand",
	MathAlignPoint:     "math alignment point",
	MathDelimited:      "delimited math",
	MathAttach:         "math attachments",
	MathFrac:           "math fraction",
	MathRoot:           "math root",
	MathPrimes:         "math primes",
	Hash:               "hash",
	LeftBrace:          "opening brace",
	RightBrace:         "closing brace",
	LeftBracket:        "opening bracket",
	RightBracket:       "closing bracket",
	LeftParen:          "opening paren",
	RightParen:         "closing paren",
	Comma:              "comma",
	Semicolon:          "semicolon",
	Colon:              "colon",
	Star:               "star",
	Underscore:         "underscore",
	Dollar:             "dollar sign",
	Plus:               "plus",
	Minus:              "minus",
	Slash:              "slash",
	Hat:                "hat",
	Prime:              "prime",
	Dot:                "dot",
	Eq:                 "equals sign",
	EqEq:               "equality operator",
	ExclEq:             "inequality operator",
	Lt:                 "less-than operator",
	LtEq:               "less-than or equal operator",
	Gt:                 "greater-than operator",
	GtEq:               "greater-than or equal operator",
	PlusEq:             "add-assign operator",
	HyphEq:             "subtract-assign operator",
	StarEq:             "multiply-assign operator",
	SlashEq:            "divide-assign operator",
	Dots:               "dots",
	Arrow:              "arrow",
	Root:               "root",
	Not:                "operator `not`",
	And:                "operator `and`",
	Or:                 "operator `or`",
	None:               "`none`",
	Auto:               "`auto`",
	Let:                "keyword `let`",
	Set:                "keyword `set`",
	Show:               "keyword `show`",
	Context:            "keyword `context`",
	If:                 "keyword `if`",
	Else:               "keyword `else`",
	For:                "keyword `for`",
	In:                 "keyword `in`",
	While:              "keyword `while`",
	Break:              "keyword `break`",
	Continue:           "keyword `continue`",
	Return:             "keyword `return`",
	Import:             "keyword `import`",
	Include:            "keyword `include`",
	As:                 "keyword `as`",
	Code:               "code",
	Ident:              "identifier",
	Bool:               "boolean",
	Int:                "integer",
	Float:              "float",
	Numeric:            "numeric value",
	Str:                "string",
	CodeBlock:          "code block",
	ContentBlock:       "content block",
	Parenthesized:      "group",
	Array:              "array",
	Dict:               "dictionary",
	Named:              "named pair",
	Keyed:              "keyed pair",
	Unary:              "unary expression",
	Binary:             "binary expression",
	FieldAccess:        "field access",
	FuncCall:           "function call",
	Args:               "call arguments",
	Spread:             "spread",
	Closure:            "closure",
	Params:             "closure parameters",
	LetBinding:         "`let` expression",
	SetRule:            "`set` expression",
	ShowRule:           "`show` expression",
	Contextual:         "`context` expression",
	Conditional:        "`if` expression",
	WhileLoop:          "while-loop expression",
	ForLoop:            "for-loop expression",
	ModuleImport:       "`import` expression",
	ImportItems:        "import items",
	ImportItemPath:     "imported item path",
	RenamedImportItem:  "renamed import item",
	ModuleInclude:      "`include` expression",
	LoopBreak:          "`break` expression",
	LoopContinue:       "`continue` expression",
	FuncReturn:         "`return` expression",
	Destructuring:      "destructuring pattern",
	DestructAssignment: "destructuring assignment expression",
}

// A human-readable name for the kind.
func (kind SyntaxKind) name() string {
	return _name[kind]
}

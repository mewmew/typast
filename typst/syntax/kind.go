package syntax

// A syntactical building block of a Typst file.
//
// Can be created by the lexer or by the parser.
type SyntaxKind uint8

const (
	// The end of token stream.
	SyntaxKindEnd SyntaxKind = iota + 1
	// An invalid sequence of characters.
	SyntaxKindError

	// A shebang: `#! ...`
	SyntaxKindShebang
	// A line comment: `// ...`.
	SyntaxKindLineComment
	// A block comment: `/* ... */`.
	SyntaxKindBlockComment

	// The contents of a file or content block.
	SyntaxKindMarkup
	// Plain text without markup.
	SyntaxKindText
	// Whitespace. Contains at most one newline in markup, as more indicate a
	// paragraph break.
	SyntaxKindSpace
	// A forced line break: `\`.
	SyntaxKindLinebreak
	// A paragraph break, indicated by one or multiple blank lines.
	SyntaxKindParbreak
	// An escape sequence: `\#`, `\u{1F5FA}`.
	SyntaxKindEscape
	// A shorthand for a unicode codepoint. For example, `~` for non-breaking
	// space or `-?` for a soft hyphen.
	SyntaxKindShorthand
	// A smart quote: `'` or `"`.
	SyntaxKindSmartQuote
	// Strong content: `*Strong*`.
	SyntaxKindStrong
	// Emphasized content: `_Emphasized_`.
	SyntaxKindEmph
	// Raw text with optional syntax highlighting: `` `...` ``.
	SyntaxKindRaw
	// A language tag at the start of raw text: ``typ ``.
	SyntaxKindRawLang
	// A raw delimiter consisting of 1 or 3+ backticks: `` ` ``.
	SyntaxKindRawDelim
	// A sequence of whitespace to ignore in a raw text: `    `.
	SyntaxKindRawTrimmed
	// A hyperlink: `https://typst.org`.
	SyntaxKindLink
	// A label: `<intro>`.
	SyntaxKindLabel
	// A reference: `@target`, `@target[..]`.
	SyntaxKindRef
	// Introduces a reference: `@target`.
	SyntaxKindRefMarker
	// A section heading: `= Introduction`.
	SyntaxKindHeading
	// Introduces a section heading: `=`, `==`, ...
	SyntaxKindHeadingMarker
	// An item in a bullet list: `- ...`.
	SyntaxKindListItem
	// Introduces a list item: `-`.
	SyntaxKindListMarker
	// An item in an enumeration (numbered list): `+ ...` or `1. ...`.
	SyntaxKindEnumItem
	// Introduces an enumeration item: `+`, `1.`.
	SyntaxKindEnumMarker
	// An item in a term list: `/ Term: Details`.
	SyntaxKindTermItem
	// Introduces a term item: `/`.
	SyntaxKindTermMarker
	// A mathematical equation: `$x$`, `$ x^2 $`.
	SyntaxKindEquation

	// The contents of a mathematical equation: `x^2 + 1`.
	SyntaxKindMath
	// A lone text fragment in math: `x`, `25`, `3.1415`, `=`, `|`, `[`.
	SyntaxKindMathText
	// An identifier in math: `pi`.
	SyntaxKindMathIdent
	// A shorthand for a unicode codepoint in math: `a <= b`.
	SyntaxKindMathShorthand
	// An alignment point in math: `&`.
	SyntaxKindMathAlignPoint
	// Matched delimiters in math: `[x + y]`.
	SyntaxKindMathDelimited
	// A base with optional attachments in math: `a_1^2`.
	SyntaxKindMathAttach
	// Grouped primes in math: `a'''`.
	SyntaxKindMathPrimes
	// A fraction in math: `x/2`.
	SyntaxKindMathFrac
	// A root in math: `√x`, `∛x` or `∜x`.
	SyntaxKindMathRoot

	// A hash that switches into code mode: `#`.
	SyntaxKindHash
	// A left curly brace, starting a code block: `{`.
	SyntaxKindLeftBrace
	// A right curly brace, terminating a code block: `}`.
	SyntaxKindRightBrace
	// A left square bracket, starting a content block: `[`.
	SyntaxKindLeftBracket
	// A right square bracket, terminating a content block: `]`.
	SyntaxKindRightBracket
	// A left round parenthesis, starting a grouped expression, collection
	// argument or parameter list: `(`.
	SyntaxKindLeftParen
	// A right round parenthesis, terminating a grouped expression, collection
	// argument or parameter list: `)`.
	SyntaxKindRightParen
	// A comma separator in a sequence: `,`.
	SyntaxKindComma
	// A semicolon terminating an expression: `;`.
	SyntaxKindSemicolon
	// A colon between name/key and value in a dictionary, argument or
	// parameter list, or between the term and body of a term list term: `:`.
	SyntaxKindColon
	// The strong text toggle, multiplication operator, and wildcard import
	// symbol: `*`.
	SyntaxKindStar
	// Toggles emphasized text and indicates a subscript in math: `_`.
	SyntaxKindUnderscore
	// Starts and ends a mathematical equation: `$`.
	SyntaxKindDollar
	// The unary plus and binary addition operator: `+`.
	SyntaxKindPlus
	// The unary negation and binary subtraction operator: `-`.
	SyntaxKindMinus
	// The division operator and fraction operator in math: `/`.
	SyntaxKindSlash
	// The superscript operator in math: `^`.
	SyntaxKindHat
	// The prime in math: `'`.
	SyntaxKindPrime
	// The field access and method call operator: `.`.
	SyntaxKindDot
	// The assignment operator: `=`.
	SyntaxKindEq
	// The equality operator: `==`.
	SyntaxKindEqEq
	// The inequality operator: `!=`.
	SyntaxKindExclEq
	// The less-than operator: `<`.
	SyntaxKindLt
	// The less-than or equal operator: `<=`.
	SyntaxKindLtEq
	// The greater-than operator: `>`.
	SyntaxKindGt
	// The greater-than or equal operator: `>=`.
	SyntaxKindGtEq
	// The add-assign operator: `+=`.
	SyntaxKindPlusEq
	// The subtract-assign operator: `-=`.
	SyntaxKindHyphEq
	// The multiply-assign operator: `*=`.
	SyntaxKindStarEq
	// The divide-assign operator: `/=`.
	SyntaxKindSlashEq
	// Indicates a spread or sink: `..`.
	SyntaxKindDots
	// An arrow between a closure's parameters and body: `=>`.
	SyntaxKindArrow
	// A root: `√`, `∛` or `∜`.
	SyntaxKindRoot

	// The `not` operator.
	SyntaxKindNot
	// The `and` operator.
	SyntaxKindAnd
	// The `or` operator.
	SyntaxKindOr
	// The `none` literal.
	SyntaxKindNone
	// The `auto` literal.
	SyntaxKindAuto
	// The `let` keyword.
	SyntaxKindLet
	// The `set` keyword.
	SyntaxKindSet
	// The `show` keyword.
	SyntaxKindShow
	// The `context` keyword.
	SyntaxKindContext
	// The `if` keyword.
	SyntaxKindIf
	// The `else` keyword.
	SyntaxKindElse
	// The `for` keyword.
	SyntaxKindFor
	// The `in` keyword.
	SyntaxKindIn
	// The `while` keyword.
	SyntaxKindWhile
	// The `break` keyword.
	SyntaxKindBreak
	// The `continue` keyword.
	SyntaxKindContinue
	// The `return` keyword.
	SyntaxKindReturn
	// The `import` keyword.
	SyntaxKindImport
	// The `include` keyword.
	SyntaxKindInclude
	// The `as` keyword.
	SyntaxKindAs

	// The contents of a code block.
	SyntaxKindCode
	// An identifier: `it`.
	SyntaxKindIdent
	// A boolean: `true`, `false`.
	SyntaxKindBool
	// An integer: `120`.
	SyntaxKindInt
	// A floating-point number: `1.2`, `10e-4`.
	SyntaxKindFloat
	// A numeric value with a unit: `12pt`, `3cm`, `2em`, `90deg`, `50%`.
	SyntaxKindNumeric
	// A quoted string: `"..."`.
	SyntaxKindStr
	// A code block: `{ let x = 1; x + 2 }`.
	SyntaxKindCodeBlock
	// A content block: `[*Hi* there!]`.
	SyntaxKindContentBlock
	// A grouped expression: `(1 + 2)`.
	SyntaxKindParenthesized
	// An array: `(1, "hi", 12cm)`.
	SyntaxKindArray
	// A dictionary: `(thickness: 3pt, dash: "solid")`.
	SyntaxKindDict
	// A named pair: `thickness: 3pt`.
	SyntaxKindNamed
	// A keyed pair: `"spacy key": true`.
	SyntaxKindKeyed
	// A unary operation: `-x`.
	SyntaxKindUnary
	// A binary operation: `a + b`.
	SyntaxKindBinary
	// A field access: `properties.age`.
	SyntaxKindFieldAccess
	// An invocation of a function or method: `f(x, y)`.
	SyntaxKindFuncCall
	// A function call's argument list: `(12pt, y)`.
	SyntaxKindArgs
	// Spread arguments or an argument sink: `..x`.
	SyntaxKindSpread
	// A closure: `(x, y) => z`.
	SyntaxKindClosure
	// A closure's parameters: `(x, y)`.
	SyntaxKindParams
	// A let binding: `let x = 1`.
	SyntaxKindLetBinding
	// A set rule: `set text(...)`.
	SyntaxKindSetRule
	// A show rule: `show heading: it => emph(it.body)`.
	SyntaxKindShowRule
	// A contextual expression: `context text.lang`.
	SyntaxKindContextual
	// An if-else conditional: `if x { y } else { z }`.
	SyntaxKindConditional
	// A while loop: `while x { y }`.
	SyntaxKindWhileLoop
	// A for loop: `for x in y { z }`.
	SyntaxKindForLoop
	// A module import: `import "utils.typ": a, b, c`.
	SyntaxKindModuleImport
	// Items to import from a module: `a, b, c`.
	SyntaxKindImportItems
	// A path to an imported name from a submodule: `a.b.c`.
	SyntaxKindImportItemPath
	// A renamed import item: `a as d`.
	SyntaxKindRenamedImportItem
	// A module include: `include "chapter1.typ"`.
	SyntaxKindModuleInclude
	// A break from a loop: `break`.
	SyntaxKindLoopBreak
	// A continue in a loop: `continue`.
	SyntaxKindLoopContinue
	// A return from a function: `return`, `return x + 1`.
	SyntaxKindFuncReturn
	// A destructuring pattern: `(x, _, ..y)`.
	SyntaxKindDestructuring
	// A destructuring assignment expression: `(x, y) = (1, 2)`.
	SyntaxKindDestructAssignment
)

var _is_grouping = map[SyntaxKind]bool{
	SyntaxKindLeftBracket:  true,
	SyntaxKindLeftBrace:    true,
	SyntaxKindLeftParen:    true,
	SyntaxKindRightBracket: true,
	SyntaxKindRightBrace:   true,
	SyntaxKindRightParen:   true,
}

// Is this a bracket, brace, or parenthesis?
func (kind SyntaxKind) is_grouping() bool {
	return _is_grouping[kind]
}

var _is_terminator = map[SyntaxKind]bool{
	SyntaxKindEnd:          true,
	SyntaxKindSemicolon:    true,
	SyntaxKindRightBrace:   true,
	SyntaxKindRightParen:   true,
	SyntaxKindRightBracket: true,
}

// Does this node terminate a preceding expression?
func (kind SyntaxKind) is_terminator() bool {
	return _is_terminator[kind]
}

var _is_block = map[SyntaxKind]bool{
	SyntaxKindCodeBlock:    true,
	SyntaxKindContentBlock: true,
}

// Is this a code or content block.
func (kind SyntaxKind) is_block() bool {
	return _is_block[kind]
}

var _is_stmt = map[SyntaxKind]bool{
	SyntaxKindLetBinding:    true,
	SyntaxKindSetRule:       true,
	SyntaxKindShowRule:      true,
	SyntaxKindModuleImport:  true,
	SyntaxKindModuleInclude: true,
}

// Does this node need termination through a semicolon or linebreak?
func (kind SyntaxKind) is_stmt() bool {
	return _is_stmt[kind]
}

var _is_keyword = map[SyntaxKind]bool{
	SyntaxKindNot:      true,
	SyntaxKindAnd:      true,
	SyntaxKindOr:       true,
	SyntaxKindNone:     true,
	SyntaxKindAuto:     true,
	SyntaxKindLet:      true,
	SyntaxKindSet:      true,
	SyntaxKindShow:     true,
	SyntaxKindContext:  true,
	SyntaxKindIf:       true,
	SyntaxKindElse:     true,
	SyntaxKindFor:      true,
	SyntaxKindIn:       true,
	SyntaxKindWhile:    true,
	SyntaxKindBreak:    true,
	SyntaxKindContinue: true,
	SyntaxKindReturn:   true,
	SyntaxKindImport:   true,
	SyntaxKindInclude:  true,
	SyntaxKindAs:       true,
}

// Is this node is a keyword.
func (kind SyntaxKind) is_keyword() bool {
	return _is_keyword[kind]
}

var _is_trivia = map[SyntaxKind]bool{
	SyntaxKindShebang:      true,
	SyntaxKindLineComment:  true,
	SyntaxKindBlockComment: true,
	SyntaxKindSpace:        true,
	SyntaxKindParbreak:     true,
}

// Whether this kind of node is automatically skipped by the parser in
// code and math mode.
func (kind SyntaxKind) is_trivia() bool {
	return _is_trivia[kind]
}

var _is_error = map[SyntaxKind]bool{
	SyntaxKindError: true,
}

// Whether this is an error.
func (kind SyntaxKind) is_error() bool {
	return _is_error[kind]
}

var _name = map[SyntaxKind]string{
	SyntaxKindEnd:                "end of tokens",
	SyntaxKindError:              "syntax error",
	SyntaxKindShebang:            "shebang",
	SyntaxKindLineComment:        "line comment",
	SyntaxKindBlockComment:       "block comment",
	SyntaxKindMarkup:             "markup",
	SyntaxKindText:               "text",
	SyntaxKindSpace:              "space",
	SyntaxKindLinebreak:          "line break",
	SyntaxKindParbreak:           "paragraph break",
	SyntaxKindEscape:             "escape sequence",
	SyntaxKindShorthand:          "shorthand",
	SyntaxKindSmartQuote:         "smart quote",
	SyntaxKindStrong:             "strong content",
	SyntaxKindEmph:               "emphasized content",
	SyntaxKindRaw:                "raw block",
	SyntaxKindRawLang:            "raw language tag",
	SyntaxKindRawTrimmed:         "raw trimmed",
	SyntaxKindRawDelim:           "raw delimiter",
	SyntaxKindLink:               "link",
	SyntaxKindLabel:              "label",
	SyntaxKindRef:                "reference",
	SyntaxKindRefMarker:          "reference marker",
	SyntaxKindHeading:            "heading",
	SyntaxKindHeadingMarker:      "heading marker",
	SyntaxKindListItem:           "list item",
	SyntaxKindListMarker:         "list marker",
	SyntaxKindEnumItem:           "enum item",
	SyntaxKindEnumMarker:         "enum marker",
	SyntaxKindTermItem:           "term list item",
	SyntaxKindTermMarker:         "term marker",
	SyntaxKindEquation:           "equation",
	SyntaxKindMath:               "math",
	SyntaxKindMathText:           "math text",
	SyntaxKindMathIdent:          "math identifier",
	SyntaxKindMathShorthand:      "math shorthand",
	SyntaxKindMathAlignPoint:     "math alignment point",
	SyntaxKindMathDelimited:      "delimited math",
	SyntaxKindMathAttach:         "math attachments",
	SyntaxKindMathFrac:           "math fraction",
	SyntaxKindMathRoot:           "math root",
	SyntaxKindMathPrimes:         "math primes",
	SyntaxKindHash:               "hash",
	SyntaxKindLeftBrace:          "opening brace",
	SyntaxKindRightBrace:         "closing brace",
	SyntaxKindLeftBracket:        "opening bracket",
	SyntaxKindRightBracket:       "closing bracket",
	SyntaxKindLeftParen:          "opening paren",
	SyntaxKindRightParen:         "closing paren",
	SyntaxKindComma:              "comma",
	SyntaxKindSemicolon:          "semicolon",
	SyntaxKindColon:              "colon",
	SyntaxKindStar:               "star",
	SyntaxKindUnderscore:         "underscore",
	SyntaxKindDollar:             "dollar sign",
	SyntaxKindPlus:               "plus",
	SyntaxKindMinus:              "minus",
	SyntaxKindSlash:              "slash",
	SyntaxKindHat:                "hat",
	SyntaxKindPrime:              "prime",
	SyntaxKindDot:                "dot",
	SyntaxKindEq:                 "equals sign",
	SyntaxKindEqEq:               "equality operator",
	SyntaxKindExclEq:             "inequality operator",
	SyntaxKindLt:                 "less-than operator",
	SyntaxKindLtEq:               "less-than or equal operator",
	SyntaxKindGt:                 "greater-than operator",
	SyntaxKindGtEq:               "greater-than or equal operator",
	SyntaxKindPlusEq:             "add-assign operator",
	SyntaxKindHyphEq:             "subtract-assign operator",
	SyntaxKindStarEq:             "multiply-assign operator",
	SyntaxKindSlashEq:            "divide-assign operator",
	SyntaxKindDots:               "dots",
	SyntaxKindArrow:              "arrow",
	SyntaxKindRoot:               "root",
	SyntaxKindNot:                "operator `not`",
	SyntaxKindAnd:                "operator `and`",
	SyntaxKindOr:                 "operator `or`",
	SyntaxKindNone:               "`none`",
	SyntaxKindAuto:               "`auto`",
	SyntaxKindLet:                "keyword `let`",
	SyntaxKindSet:                "keyword `set`",
	SyntaxKindShow:               "keyword `show`",
	SyntaxKindContext:            "keyword `context`",
	SyntaxKindIf:                 "keyword `if`",
	SyntaxKindElse:               "keyword `else`",
	SyntaxKindFor:                "keyword `for`",
	SyntaxKindIn:                 "keyword `in`",
	SyntaxKindWhile:              "keyword `while`",
	SyntaxKindBreak:              "keyword `break`",
	SyntaxKindContinue:           "keyword `continue`",
	SyntaxKindReturn:             "keyword `return`",
	SyntaxKindImport:             "keyword `import`",
	SyntaxKindInclude:            "keyword `include`",
	SyntaxKindAs:                 "keyword `as`",
	SyntaxKindCode:               "code",
	SyntaxKindIdent:              "identifier",
	SyntaxKindBool:               "boolean",
	SyntaxKindInt:                "integer",
	SyntaxKindFloat:              "float",
	SyntaxKindNumeric:            "numeric value",
	SyntaxKindStr:                "string",
	SyntaxKindCodeBlock:          "code block",
	SyntaxKindContentBlock:       "content block",
	SyntaxKindParenthesized:      "group",
	SyntaxKindArray:              "array",
	SyntaxKindDict:               "dictionary",
	SyntaxKindNamed:              "named pair",
	SyntaxKindKeyed:              "keyed pair",
	SyntaxKindUnary:              "unary expression",
	SyntaxKindBinary:             "binary expression",
	SyntaxKindFieldAccess:        "field access",
	SyntaxKindFuncCall:           "function call",
	SyntaxKindArgs:               "call arguments",
	SyntaxKindSpread:             "spread",
	SyntaxKindClosure:            "closure",
	SyntaxKindParams:             "closure parameters",
	SyntaxKindLetBinding:         "`let` expression",
	SyntaxKindSetRule:            "`set` expression",
	SyntaxKindShowRule:           "`show` expression",
	SyntaxKindContextual:         "`context` expression",
	SyntaxKindConditional:        "`if` expression",
	SyntaxKindWhileLoop:          "while-loop expression",
	SyntaxKindForLoop:            "for-loop expression",
	SyntaxKindModuleImport:       "`import` expression",
	SyntaxKindImportItems:        "import items",
	SyntaxKindImportItemPath:     "imported item path",
	SyntaxKindRenamedImportItem:  "renamed import item",
	SyntaxKindModuleInclude:      "`include` expression",
	SyntaxKindLoopBreak:          "`break` expression",
	SyntaxKindLoopContinue:       "`continue` expression",
	SyntaxKindFuncReturn:         "`return` expression",
	SyntaxKindDestructuring:      "destructuring pattern",
	SyntaxKindDestructAssignment: "destructuring assignment expression",
}

// A human-readable name for the kind.
func (kind SyntaxKind) name() string {
	return _name[kind]
}

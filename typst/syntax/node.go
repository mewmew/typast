package syntax

import (
	"fmt"
	"io"
	"iter"
	"slices"
	"strings"

	"github.com/mewmew/typast/internal/mathx"
	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
	overflow "github.com/mrtkp9993/go-overflow"
	"github.com/pkg/errors"
)

// --- [ SyntaxNode ] ----------------------------------------------------------

// A node in the untyped syntax tree.
type SyntaxNode struct {
	Repr Repr
}

func (node *SyntaxNode) SyntaxKind() SyntaxKind {
	return node.Repr.SyntaxKind()
}

// The three internal representations.
type Repr interface {
	isSyntaxNode()
	clone() Repr

	SyntaxKind() SyntaxKind
}

func (*LeafNode) isSyntaxNode()  {} // A leaf node.
func (*InnerNode) isSyntaxNode() {} // An inner node.
func (*ErrorNode) isSyntaxNode() {} // An error node.

func (node *LeafNode) SyntaxKind() SyntaxKind {
	return node.Kind
}

func (node *InnerNode) SyntaxKind() SyntaxKind {
	return node.Kind
}

func (*ErrorNode) SyntaxKind() SyntaxKind {
	return SyntaxKindError
}

// Create a new leaf node.
//
// leaf
func NewLeaf(kind SyntaxKind, text string) *SyntaxNode {
	return &SyntaxNode{
		Repr: NewLeafNode(kind, text),
	}
}

// Create a new inner node with children.
//
// inner
func NewInner(kind SyntaxKind, children []*SyntaxNode) *SyntaxNode {
	return &SyntaxNode{
		Repr: NewInnerNode(kind, children),
	}
}

// Create a new error node.
//
// error
func NewError(error *SyntaxError, text string) *SyntaxNode {
	return &SyntaxNode{
		Repr: NewErrorNode(error, text),
	}
}

// Create a dummy node of the given kind.
//
// Panics if `kind` is `SyntaxKind::Error`.
//
// placeholder
func SyntaxNode_placeholder(kind SyntaxKind) *SyntaxNode {
	if kind == SyntaxKindError {
		panic("cannot create error placeholder")
	}
	return &SyntaxNode{
		Repr: &LeafNode{
			Kind: kind,
			Text: "",
			Span: NewDetachedSpan(),
		},
	}
}

// The type of the node.
func (node *SyntaxNode) kind() SyntaxKind {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return repr.Kind
	case *InnerNode:
		return repr.Kind
	case *ErrorNode:
		return SyntaxKindError
	}
	panic("unreachable")
}

// Return `true` if the length is 0.
func (node *SyntaxNode) is_empty() bool {
	return node.len() == 0
}

// The byte length of the node in the source text.
func (node *SyntaxNode) len() uint {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return repr.len()
	case *InnerNode:
		return repr.Len
	case *ErrorNode:
		return repr.len()
	}
	panic("unreachable")
}

// The span of the node.
func (node *SyntaxNode) span() Span {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return repr.Span
	case *InnerNode:
		return repr.Span
	case *ErrorNode:
		return repr.Error.Span
	}
	panic("unreachable")
}

// The text of the node if it is a leaf or error node.
//
// Returns the empty string if this is an inner node.
func (node *SyntaxNode) text() string {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return repr.Text
	case *InnerNode:
		return ""
	case *ErrorNode:
		return repr.Text
	}
	panic("unreachable")
}

// Extract the text from the node.
//
// Builds the string if this is an inner node.
func (node *SyntaxNode) into_text() string {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return repr.Text
	case *InnerNode:
		buf := &strings.Builder{}
		for i, child := range repr.Children {
			if i != 0 {
				buf.WriteString(" ")
			}
			buf.WriteString(child.into_text())
		}
		return buf.String()
	case *ErrorNode:
		return repr.Text
	}
	panic("unreachable")
}

// The node's children.
func (node *SyntaxNode) children() []*SyntaxNode {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return nil
	case *ErrorNode:
		return nil
	case *InnerNode:
		return repr.Children
	}
	panic("unreachable")
}

// Whether the node or its children contain an error.
func (node *SyntaxNode) erroneous() bool {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return false
	case *InnerNode:
		return repr.Erroneous
	case *ErrorNode:
		return true
	}
	panic("unreachable")
}

// The error messages for this node and its descendants.
func (node *SyntaxNode) errors() []*SyntaxError {
	if !node.erroneous() {
		return nil
	}

	switch repr := node.Repr.(type) {
	case *ErrorNode:
		return []*SyntaxError{repr.Error.clone()}
	default:
		var error_nodes []*SyntaxNode
		for _, child := range node.children() {
			if child.erroneous() {
				error_nodes = append(error_nodes, child)
			}
		}
		var errors []*SyntaxError
		for _, error_node := range error_nodes {
			errors = append(errors, error_node.errors()...)
		}
		return errors
	}
}

// Add a user-presentable hint if this is an error node.
func (node *SyntaxNode) hint(hint string) {
	switch repr := node.Repr.(type) {
	case *ErrorNode:
		repr.hint(hint)
	}
}

// Set a synthetic span for the node and all its descendants.
func (node *SyntaxNode) synthesize(span Span) {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		repr.Span = span
	case *InnerNode:
		repr.synthesize(span)
	case *ErrorNode:
		repr.Error.Span = span
	}
}

// Whether the two syntax nodes are the same apart from spans.
func (node *SyntaxNode) spanless_eq(other *SyntaxNode) bool {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		_other, ok := other.Repr.(*LeafNode)
		if !ok {
			return false
		}
		return repr.spanless_eq(_other)
	case *InnerNode:
		_other, ok := other.Repr.(*InnerNode)
		if !ok {
			return false
		}
		return repr.spanless_eq(_other)
	case *ErrorNode:
		_other, ok := other.Repr.(*ErrorNode)
		if !ok {
			return false
		}
		return repr.spanless_eq(_other)
	}
	return false
}

func (node *SyntaxNode) clone() *SyntaxNode {
	return &SyntaxNode{
		Repr: node.Repr.clone(),
	}
}

// Convert the child to another kind.
//
// Don't use this for converting to an error!
func (node *SyntaxNode) convert_to_kind(kind SyntaxKind) {
	if kind.is_error() {
		panic("error kind in leaf node")
	}
	switch repr := node.Repr.(type) {
	case *LeafNode:
		repr.Kind = kind
	case *InnerNode:
		repr.Kind = kind
	case *ErrorNode:
		panic("cannot convert error")
	}
}

// Convert the child to an error, if it isn't already one.
func (node *SyntaxNode) convert_to_error(message string) {
	if node.kind().is_error() {
		return
	}
	text := node.into_text()
	*node = *NewError(NewSyntaxError(message), text)
}

// Convert the child to an error stating that the given thing was
// expected, but the current kind was found.
func (node *SyntaxNode) expected(expected string) {
	kind := node.kind()
	node.convert_to_error(fmt.Sprintf("expected %v, found %v", expected, kind.name()))
	if kind.is_keyword() && (expected == "identifier" || expected == "pattern") {
		hint := fmt.Sprintf("keyword `%[1]v` is not allowed as an identifier; try `%[1]v_` instead", node.text())
		node.hint(hint)
	}
}

// Convert the child to an error stating it was unexpected.
func (node *SyntaxNode) unexpected() {
	node.convert_to_error(fmt.Sprintf("unexpected %v", node.kind().name()))
}

// Assign spans to each node.
//
// numberize
func (node *SyntaxNode) numberize(id FileID, within ranges.Range) error {
	if within.Start >= within.End {
		return ErrUnnumberable
	}

	mid, _ := NewSpanFromNumber(id, (within.Start+within.End)/2)
	switch repr := node.Repr.(type) {
	case *LeafNode:
		repr.Span = mid
	case *InnerNode:
		repr.numberize(id, option.None[ranges.Range](), within)
	case *ErrorNode:
		repr.Error.Span = mid
	}

	return nil
}

// Whether this is a leaf node.
func (node *SyntaxNode) is_leaf() bool {
	_, ok := node.Repr.(*LeafNode)
	return ok
}

// The number of descendants, including the node itself.
//
// descendants
func (node *SyntaxNode) descendants() uint {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return 1
	case *ErrorNode:
		return 1
	case *InnerNode:
		return repr.Descendants
	}
	panic("unreachable")
}

// Replaces a range of children with a replacement.
//
// May have mutated the children if it returns `Err(_)`.
func (node *SyntaxNode) replace_children(_range ranges.Range, replacement []*SyntaxNode) error {
	if inner, ok := node.Repr.(*InnerNode); ok {
		return inner.replace_children(_range, replacement)
	}
	return nil
}

// Update this node after changes were made to one of its children.
func (node *SyntaxNode) update_parent(prev_len, new_len, prev_descendants, new_descendants uint) {
	inner, ok := node.Repr.(*InnerNode)
	if !ok {
		return
	}
	inner.update_parent(prev_len, new_len, prev_descendants, new_descendants)
}

// The upper bound of assigned numbers in this subtree.
func (node *SyntaxNode) upper() uint64 {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return repr.Span.Number() + 1
	case *InnerNode:
		return repr.Upper
	case *ErrorNode:
		return repr.Error.Span.Number() + 1
	}
	panic("unreachable")
}

func (node *SyntaxNode) String() string {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		return repr.String()
	case *InnerNode:
		return repr.String()
	case *ErrorNode:
		return repr.String()
	}
	panic("unreachable")
}

func SyntaxNode_Default() *SyntaxNode {
	return &SyntaxNode{
		Repr: NewLeafNode(SyntaxKindEnd, ""),
	}
}

// --- [/ SyntaxNode ] ---------------------------------------------------------

// --- [ LeafNode ] ------------------------------------------------------------

// A leaf node in the untyped syntax tree.
type LeafNode struct {
	// What kind of node this is (each kind would have its own struct in a
	// strongly typed AST).
	Kind SyntaxKind
	// The source text of the node.
	Text string
	// The node's span.
	Span Span
}

// Create a new leaf node.
//
// new
func NewLeafNode(kind SyntaxKind, text string) *LeafNode {
	if kind.is_error() {
		panic("error kind in leaf node")
	}
	return &LeafNode{
		Kind: kind,
		Text: text,
		Span: NewDetachedSpan(),
	}
}

// The byte length of the node in the source text.
func (node *LeafNode) len() uint {
	return uint(len(node.Text))
}

// Whether the two leaf nodes are the same apart from spans.
func (node *LeafNode) spanless_eq(other *LeafNode) bool {
	return node.Kind == other.Kind && node.Text == other.Text
}

func (node *LeafNode) String() string {
	return fmt.Sprintf("%v: %v", node.Kind, node.Text)
}

func (node *LeafNode) clone() Repr {
	return &LeafNode{
		Kind: node.Kind,
		Text: node.Text,
		Span: node.Span,
	}
}

// --- [/ LeafNode ] -----------------------------------------------------------

// --- [ InnerNode ] -----------------------------------------------------------

// An inner node in the untyped syntax tree.
type InnerNode struct {
	// What kind of node this is (each kind would have its own struct in a
	// strongly typed AST).
	Kind SyntaxKind
	// The byte length of the node in the source.
	Len uint
	// The node's span.
	Span Span
	// The number of nodes in the whole subtree, including this node.
	Descendants uint
	// Whether this node or any of its children are erroneous.
	Erroneous bool
	// The upper bound of this node's numbering range.
	Upper uint64
	// This node's children, losslessly make up this node.
	Children []*SyntaxNode
}

// Create a new inner node with the given kind and children.
//
// new
func NewInnerNode(kind SyntaxKind, children []*SyntaxNode) *InnerNode {
	if kind.is_error() {
		panic("error kind in inner node")
	}

	length := uint(0)
	descendants := uint(1)
	erroneous := false

	for _, child := range children {
		length += child.len()
		descendants += child.descendants()
		if child.erroneous() {
			erroneous = true
		}
	}

	return &InnerNode{
		Kind:        kind,
		Len:         length,
		Span:        NewDetachedSpan(),
		Descendants: descendants,
		Erroneous:   erroneous,
		Upper:       0,
		Children:    children,
	}
}

// Set a synthetic span for the node and all its descendants.
func (node *InnerNode) synthesize(span Span) {
	node.Span = span
	node.Upper = span.Number()
	for _, child := range node.Children {
		child.synthesize(span)
	}
}

// Assign span numbers `within` an interval to this node's subtree or just
// a `range` of its children.
func (node *InnerNode) numberize(id FileID, _range option.Option[ranges.Range], within ranges.Range) error {
	// Determine how many nodes we will number.
	descendants := uint(0)
	if _range, ok := _range.Get(); ok {
		if _range.IsEmpty() {
			return nil
		}
		for _, child := range node.Children[_range.Start:_range.End] {
			descendants += child.descendants()
		}
	} else {
		descendants = node.Descendants
	}

	// Determine the distance between two neighbouring assigned numbers. If
	// possible, we try to fit all numbers into the left half of `within`
	// so that there is space for future insertions.
	space := within.End - within.Start
	stride := space / (2 * uint64(descendants))
	if stride == 0 {
		stride = space / uint64(node.Descendants)
		if stride == 0 {
			return ErrUnnumberable
		}
	}

	// Number the node itself.
	start := within.Start
	if !_range.IsPresent() {
		end := start + stride
		node.Span, _ = NewSpanFromNumber(id, (start+end)/2)
		node.Upper = within.End
		start = end
	}

	// Number the children.
	length := len(node.Children)
	_range_start := uint64(0)
	_range_end := uint64(length)
	if _range, ok := _range.Get(); ok {
		_range_start = _range.Start
		_range_end = _range.End
	}
	for _, child := range node.Children[_range_start:_range_end] {
		end := start + uint64(child.descendants())*stride
		child.numberize(id, ranges.NewRange(start, end))
		start = end
	}

	return nil
}

// Whether the two inner nodes are the same apart from spans.
func (node *InnerNode) spanless_eq(other *InnerNode) bool {
	if node.Kind != other.Kind {
		return false
	}
	if node.Len != other.Len {
		return false
	}
	if node.Descendants != other.Descendants {
		return false
	}
	if node.Erroneous != other.Erroneous {
		return false
	}
	if len(node.Children) != len(other.Children) {
		return false
	}
	for i, a := range node.Children {
		b := other.Children[i]
		if !a.spanless_eq(b) {
			return false
		}
	}
	return true
}

// Replaces a range of children with a replacement.
//
// May have mutated the children if it returns `Err(_)`.
func (node *InnerNode) replace_children(_range ranges.Range, replacement []*SyntaxNode) error {
	id, ok := node.Span.ID()
	if !ok {
		return ErrUnnumberable
	}
	replacement_range := ranges.NewRange(0, uint64(len(replacement)))

	// Trim off common prefix.
	for _range.Start < _range.End && replacement_range.Start < replacement_range.End && node.Children[_range.Start].spanless_eq(replacement[replacement_range.Start]) {
		_range.Start += 1
		replacement_range.Start += 1
	}

	// Trim off common suffix.
	for _range.Start < _range.End && replacement_range.Start < replacement_range.End && node.Children[_range.End-1].spanless_eq(replacement[replacement_range.End-1]) {
		_range.End -= 1
		replacement_range.End -= 1
	}

	// TODO: double-check that conversion from Rust to Go was correct.
	replacement_vec := replacement
	replacement = replacement_vec[replacement_range.Start:replacement_range.End]
	superseded := node.Children[_range.Start:_range.End]

	// Compute the new byte length.
	for _, r := range replacement {
		node.Len += r.len()
	}
	for _, s := range superseded {
		node.Len -= s.len()
	}

	// Compute the new number of descendants.
	for _, r := range replacement {
		node.Descendants += r.descendants()
	}
	for _, s := range superseded {
		node.Descendants -= s.descendants()
	}

	// Determine whether we're still erroneous after the replacement. That's
	// the case if
	// - any of the new nodes is erroneous,
	// - or if we were erroneous before due to a non-superseded node.
	erroneous := false
	for _, r := range replacement {
		if r.erroneous() {
			erroneous = true
		}
	}
	if node.Erroneous {
		for _, c := range node.Children[:replacement_range.Start] {
			if c.erroneous() {
				erroneous = true
			}
		}
		for _, c := range node.Children[replacement_range.End:] {
			if c.erroneous() {
				erroneous = true
			}
		}
	}
	node.Erroneous = erroneous

	// TODO: double-check that translation from Rust to Go was correct.

	// Perform the replacement.
	node.Children = slices.Replace(node.Children, int(_range.Start), int(_range.End), replacement_vec[replacement_range.Start:replacement_range.End]...)
	_range.End = _range.Start + replacement_range.Len()

	// Renumber the new children. Retries until it works, taking
	// exponentially more children into account.
	left := uint64(0)
	right := uint64(0)
	max_left := _range.Start
	max_right := uint64(len(node.Children)) - _range.End
	for {
		renumber := ranges.NewRange(_range.Start-left, _range.End+right)

		// TODO: double-check that translation from Rust to Go was correct.

		// The minimum assignable number is either
		// - the upper bound of the node right before the to-be-renumbered
		//   children,
		// - or this inner node's span number plus one if renumbering starts
		//   at the first child.
		var start_number uint64
		if start, fail := overflow.SubUint64(renumber.Start, 1); !fail {
			start_number = node.Children[start].upper()
		} else { // underflow
			start_number = node.Span.Number() + 1
		}

		// TODO: double-check that translation from Rust to Go was correct.

		// The upper bound for renumbering is either
		// - the span number of the first child after the to-be-renumbered
		//   children,
		// - or this node's upper bound if renumbering ends behind the last
		//   child.
		var end_number uint64
		if renumber.End < uint64(len(node.Children)) {
			end_number = node.Children[renumber.End].span().Number()
		} else {
			end_number = node.Upper
		}

		// Try to renumber.
		within := ranges.NewRange(start_number, end_number)
		if node.numberize(id, option.Some(renumber), within) != nil {
			return nil
		}

		// If it didn't even work with all children, we give up.
		if left == max_left && right == max_right {
			return ErrUnnumberable
		}

		// Exponential expansion to both sides.
		left = min(mathx.NextPowerOfTwo(left+1), max_left)
		right = min(mathx.NextPowerOfTwo(right+1), max_right)
	}
}

// Update this node after changes were made to one of its children.
func (node *InnerNode) update_parent(prev_len, new_len, prev_descendants, new_descendants uint) {
	node.Len = node.Len + new_len - prev_len
	node.Descendants = node.Descendants + new_descendants - prev_descendants
	node.Erroneous = false
	for _, child := range node.Children {
		if child.erroneous() {
			node.Erroneous = true
		}
	}
}

func (node *InnerNode) String() string {
	buf := &strings.Builder{}
	fmt.Fprintf(buf, "%v: %v", node.Kind, node.Len)
	for _, child := range node.Children {
		fmt.Fprintf(buf, " %v", child)
	}
	return buf.String()
}

func (node *InnerNode) clone() Repr {
	var children []*SyntaxNode
	for _, child := range node.Children {
		children = append(children, child.clone())
	}
	return &InnerNode{
		Kind:        node.Kind,
		Len:         node.Len,
		Span:        node.Span,
		Descendants: node.Descendants,
		Erroneous:   node.Erroneous,
		Upper:       node.Upper,
		Children:    children,
	}
}

// --- [/ InnerNode ] ----------------------------------------------------------

// --- [ ErrorNode ] -----------------------------------------------------------

// An error node in the untyped syntax tree.
type ErrorNode struct {
	// The source text of the node.
	Text string
	// The syntax error.
	Error *SyntaxError
}

// Create new error node.
//
// new
func NewErrorNode(error *SyntaxError, text string) *ErrorNode {
	return &ErrorNode{
		Text:  text,
		Error: error,
	}
}

// The byte length of the node in the source text.
func (node *ErrorNode) len() uint {
	return uint(len(node.Text))
}

// Add a user-presentable hint to this error node.
func (node *ErrorNode) hint(hint string) {
	node.Error.Hints = append(node.Error.Hints, hint)
}

// Whether the two leaf nodes are the same apart from spans.
func (node *ErrorNode) spanless_eq(other *ErrorNode) bool {
	return node.Text == other.Text && node.Error.spanless_eq(other.Error)
}

func (node *ErrorNode) String() string {
	return fmt.Sprintf("Error: %v (%v)", node.Text, node.Error.Message)
}

func (node *ErrorNode) clone() Repr {
	return &ErrorNode{
		Text:  node.Text,
		Error: node.Error.clone(),
	}
}

// --- [/ ErrorNode ] ----------------------------------------------------------

// --- [ SyntaxError ] ---------------------------------------------------------

// A syntactical error.
type SyntaxError struct {
	// The node's span.
	Span Span
	// The error message.
	Message string
	// Additional hints to the user, indicating how this error could be avoided
	// or worked around.
	Hints []string
}

// Create a new detached syntax error.
//
// new
func NewSyntaxError(message string) *SyntaxError {
	return &SyntaxError{
		Span:    NewDetachedSpan(),
		Message: message,
		Hints:   nil,
	}
}

// Whether the two errors are the same apart from spans.
func (node *SyntaxError) spanless_eq(other *SyntaxError) bool {
	return node.Message == other.Message && slices.Equal(node.Hints, other.Hints)
}

func (node *SyntaxError) clone() *SyntaxError {
	return &SyntaxError{
		Span:    node.Span,
		Message: node.Message,
		Hints:   slices.Clone(node.Hints),
	}
}

// --- [/ SyntaxError ] --------------------------------------------------------

// --- [ LinkedNode ] ----------------------------------------------------------

// A syntax node in a context.
//
// Knows its exact offset in the file and provides access to its
// children, parent and siblings.
//
// **Note that all sibling and leaf accessors skip over trivia!**
type LinkedNode struct {
	node *SyntaxNode
	// This node's parent.
	parent option.Option[*LinkedNode]
	// The index of this node in its parent's children list.
	index uint
	// The absolute byte offset of this node in the source file.
	offset uint
}

// Start a new traversal at a root node.
//
// new
func NewLinkedNode(root *SyntaxNode) *LinkedNode {
	return &LinkedNode{
		node:   root,
		parent: option.None[*LinkedNode](),
		index:  0,
		offset: 0,
	}
}

// Get the contained syntax node.
func (link *LinkedNode) get() *SyntaxNode {
	return link.node
}

// The byte range of this node in the source file.
//
// range
func (link *LinkedNode) Range() ranges.Range {
	return ranges.NewRange(uint64(link.offset), uint64(link.offset+link.node.len()))
}

// An iterator over this node's children.
func (link *LinkedNode) children() *LinkedChildren {
	return &LinkedChildren{
		parent:    link.clone(),
		_children: link.node.children(), // NOTE: was `iter`
		front:     link.offset,
		back:      link.offset + link.node.len(), // NOTE: was `link.len()`
	}
}

// Find a descendant with the given span.
func (link *LinkedNode) find(span Span) option.Option[*LinkedNode] {
	// NOTE: was `link.span()`
	if link.node.span() == span {
		return option.Some(link.clone())
	}

	if inner, ok := link.node.Repr.(*InnerNode); ok {
		// The parent of a subtree has a smaller span number than all of its
		// descendants. Therefore, we can bail out early if the target span's
		// number is smaller than our number.
		if span.Number() < inner.Span.Number() {
			return option.None[*LinkedNode]()
		}

		children := link.children()
		for i, child := range children.Children() {
			// Every node in this child's subtree has a smaller span number than
			// the next sibling. Therefore we only need to recurse if the next
			// sibling's span number is larger than the target span's number.
			ok := true
			if i < len(children._children) {
				next := children._children[i+1]
				ok = next.span().Number() > span.Number()
			}
			if ok {
				found := child.find(span)
				if found.IsPresent() {
					return found
				}
			}
		}
	}

	return option.None[*LinkedNode]()
}

func (link *LinkedNode) clone() *LinkedNode {
	return &LinkedNode{
		node:   link.node.clone(),
		parent: link.parent.Clone(), // TODO: implement deep clone?
		index:  link.index,
		offset: link.offset,
	}
}

// Access to parents and siblings.

// Get the first previous non-trivia sibling node.
func (link *LinkedNode) prev_sibling() option.Option[*LinkedNode] {
	parent, ok := link.parent.Get()
	if !ok {
		return option.None[*LinkedNode]()
	}
	index, fail := overflow.SubUint64(uint64(link.index), 1)
	if fail { // underflow
		return option.None[*LinkedNode]()
	}
	children := parent.node.children()
	if int(index) >= len(children) {
		return option.None[*LinkedNode]()
	}
	node := children[index]
	offset := link.offset - node.len()
	prev := &LinkedNode{
		node:   node,
		parent: link.parent.Clone(), // TODO: implement deep clone?
		index:  uint(index),
		offset: offset,
	}
	// NOTE: was `prev.kind()`
	if prev.node.kind().is_trivia() {
		return prev.prev_sibling()
	} else {
		return option.Some(prev)
	}
}

// Get the next non-trivia sibling node.
func (link *LinkedNode) next_sibling() option.Option[*LinkedNode] {
	parent, ok := link.parent.Get()
	if !ok {
		return option.None[*LinkedNode]()
	}
	index, fail := overflow.AddUint64(uint64(link.index), 1)
	if fail { // overflow
		return option.None[*LinkedNode]()
	}
	children := parent.node.children()
	if int(index) >= len(children) {
		return option.None[*LinkedNode]()
	}
	node := children[index]
	offset := link.offset + link.node.len()
	next := &LinkedNode{
		node:   node,
		parent: link.parent.Clone(), // TODO: implement deep clone?
		index:  uint(index),
		offset: offset,
	}
	// NOTE: was `next.kind()`
	if next.node.kind().is_trivia() {
		return next.next_sibling()
	} else {
		return option.Some(next)
	}
}

// Get the kind of this node's parent.
func (link *LinkedNode) parent_kind() option.Option[SyntaxKind] {
	parent, ok := link.parent.Get()
	if !ok {
		return option.None[SyntaxKind]()
	}
	return option.Some(parent.node.kind())
}

// Get the kind of this node's first previous non-trivia sibling.
func (link *LinkedNode) prev_sibling_kind() option.Option[SyntaxKind] {
	prev_sibling, ok := link.prev_sibling().Get()
	if !ok {
		return option.None[SyntaxKind]()
	}
	return option.Some(prev_sibling.node.kind())
}

// Get the kind of this node's next non-trivia sibling.
func (link *LinkedNode) next_sibling_kind() option.Option[SyntaxKind] {
	next_sibling, ok := link.next_sibling().Get()
	if !ok {
		return option.None[SyntaxKind]()
	}
	return option.Some(next_sibling.node.kind())
}

// --- [ LinkedNode ] ----------------------------------------------------------

// Indicates whether the cursor is before the related byte index, or after.
type Side uint8

const (
	SideBefore Side = iota + 1
	SideAfter
)

// Access to leaves.

// Get the rightmost non-trivia leaf before this node.
func (link *LinkedNode) prev_leaf() option.Option[*LinkedNode] {
	node := link.clone()
	for {
		prev, ok := node.prev_sibling().Get()
		if !ok {
			break
		}
		if leaf, ok := prev.rightmost_leaf().Get(); ok {
			return option.Some(leaf)
		}
		node = prev
	}
	if parent, ok := link.parent.Get(); ok {
		return parent.prev_leaf()
	}
	return option.None[*LinkedNode]()
}

// Find the leftmost contained non-trivia leaf.
func (link *LinkedNode) leftmost_leaf() option.Option[*LinkedNode] {
	if link.node.is_leaf() && !link.node.kind().is_trivia() && !link.node.kind().is_error() {
		return option.Some(link.clone())
	}

	for _, child := range link.children().Children() {
		if leaf, ok := child.leftmost_leaf().Get(); ok {
			return option.Some(leaf)
		}
	}

	return option.None[*LinkedNode]()
}

// Get the leaf immediately before the specified byte offset.
func (link *LinkedNode) leaf_before(cursor uint) option.Option[*LinkedNode] {
	if len(link.node.children()) == 0 && cursor <= link.offset+link.node.len() {
		return option.Some(link.clone())
	}

	offset := link.offset
	count := len(link.node.children())
	for i, child := range link.children().Children() {
		length := child.node.len()
		if (offset < cursor && cursor <= offset+length) || (offset == cursor && i+1 == count) {
			return child.leaf_before(cursor)
		}
		offset += length
	}

	return option.None[*LinkedNode]()
}

// Get the leaf after the specified byte offset.
func (link *LinkedNode) leaf_after(cursor uint) option.Option[*LinkedNode] {
	if len(link.node.children()) == 0 && cursor < link.offset+link.node.len() {
		return option.Some(link.clone())
	}

	offset := link.offset
	for _, child := range link.children().Children() {
		length := child.node.len()
		if offset <= cursor && cursor < offset+length {
			return child.leaf_after(cursor)
		}
		offset += length
	}

	return option.None[*LinkedNode]()
}

// Get the leaf at the specified byte offset.
func (link *LinkedNode) leaf_at(cursor uint, side Side) option.Option[*LinkedNode] {
	switch side {
	case SideBefore:
		return link.leaf_before(cursor)
	case SideAfter:
		return link.leaf_after(cursor)
	}
	panic("unreachable")
}

// Find the rightmost contained non-trivia leaf.
func (link *LinkedNode) rightmost_leaf() option.Option[*LinkedNode] {
	if link.node.is_leaf() && !link.node.kind().is_trivia() {
		return option.Some(link.clone())
	}

	for _, child := range link.children().RevChildren() {
		if leaf, ok := child.rightmost_leaf().Get(); ok {
			return option.Some(leaf)
		}
	}

	return option.None[*LinkedNode]()
}

// Get the leftmost non-trivia leaf after this node.
func (link *LinkedNode) next_leaf() option.Option[*LinkedNode] {
	node := link.clone()
	for {
		next, ok := node.next_sibling().Get()
		if !ok {
			break
		}
		if leaf, ok := next.leftmost_leaf().Get(); ok {
			return option.Some(leaf)
		}
		node = next
	}
	if parent, ok := link.parent.Get(); ok {
		return parent.next_leaf()
	}
	return option.None[*LinkedNode]()
}

func (link *LinkedNode) String() string {
	return link.node.String()
}

// --- [ LinkedChildren ] ------------------------------------------------------

// An iterator over the children of a linked node.
type LinkedChildren struct {
	parent    *LinkedNode
	_children []*SyntaxNode // NOTE: was `iter`
	front     uint
	back      uint
}

func (l *LinkedChildren) Children() iter.Seq2[int, *LinkedNode] {
	return func(yield func(int, *LinkedNode) bool) {
		for index, child := range l._children {
			offset := l.front
			l.front += child.len()
			link := &LinkedNode{
				node:   child,
				parent: option.Some(l.parent.clone()),
				index:  uint(index),
				offset: offset,
			}
			if !yield(index, link) {
				return
			}
		}
	}
}

func (l *LinkedChildren) RevChildren() iter.Seq2[int, *LinkedNode] {
	return func(yield func(int, *LinkedNode) bool) {
		for index := len(l._children) - 1; index >= 0; index-- {
			child := l._children[index]
			offset := l.back
			l.back -= child.len()
			link := &LinkedNode{
				node:   child,
				parent: option.Some(l.parent.clone()),
				index:  uint(index),
				offset: offset,
			}
			if !yield(index, link) {
				return
			}
		}
	}
}

// --- [/ LinkedChildren ] -----------------------------------------------------

// Indicates that a node cannot be numbered within a given interval.
var ErrUnnumberable = errors.New("cannot number within this interval")

// ### [ Helper functions ] ####################################################

func PrintRoot(root *SyntaxNode) {
	depth := 0
	printTree(root, depth)
}

func printTree(n *SyntaxNode, depth int) {
	pad := strings.Repeat("  ", depth)
	depth++
	switch repr := n.Repr.(type) {
	case *LeafNode:
		fmt.Printf("%sleaf (kind=%q) text=%q\n", pad, repr.Kind, repr.Text)
	case *InnerNode:
		fmt.Printf("%sinner (kind=%q)\n", pad, repr.Kind)
		for _, child := range repr.Children {
			printTree(child, depth)
		}
	case *ErrorNode:
		fmt.Printf("%serror (text=%q) error=%q\n", pad, repr.Text, repr.Error.Message)
	}
}

func PrintNode(out io.Writer, node *SyntaxNode) {
	switch repr := node.Repr.(type) {
	case *LeafNode:
		fmt.Fprint(out, repr.Text)
	case *InnerNode:
		for _, child := range repr.Children {
			PrintNode(out, child)
		}
	case *ErrorNode:
		fmt.Fprint(out, repr.Text) // TODO: remove? return error or print warning?
	}
}

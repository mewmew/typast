package syntax

import (
	"fmt"
	"math"

	"github.com/mewmew/typast/internal/option"
	"github.com/mewmew/typast/internal/ranges"
)

// Refresh the given syntax node with as little parsing as possible.
//
// Takes the new text, the range in the old text that was replaced and the
// length of the replacement and returns the range in the new text that was
// ultimately reparsed.
//
// The high-level API for this function is
// [`Source::edit`](crate::Source::edit).
func reparse(
	root *SyntaxNode,
	text string,
	replaced ranges.Range,
	replacement_len uint,
) ranges.Range {
	if r, ok := try_reparse(text, replaced, replacement_len, option.None[SyntaxKind](), root, 0).Get(); ok {
		return r
	}
	id := root.span().id()
	*root = *parse(text)
	if id, ok := id.Get(); ok {
		if err := root.numberize(id, SpanFULL); err != nil {
			panic(err)
		}
	}
	return ranges.NewRange(0, uint64(len(text)))
}

// Try to reparse inside the given node.
func try_reparse(
	text string,
	replaced ranges.Range,
	replacement_len uint,
	parent_kind option.Option[SyntaxKind],
	node *SyntaxNode,
	offset uint,
) option.Option[ranges.Range] {
	// The range of children which overlap with the edit.
	overlap := ranges.NewRange(math.MaxUint64, 0)
	cursor := offset
	node_kind := node.kind()

	for i, child := range node.children() {
		prev_range := ranges.NewRange(uint64(cursor), uint64(cursor+child.len()))
		prev_len := child.len()
		prev_desc := child.descendants()

		// Does the child surround the edit?
		// If so, try to reparse within it or itself.
		if !child.is_leaf() && includes(prev_range, replaced) {
			new_len := prev_len + replacement_len - uint(replaced.Len())
			new_range := ranges.NewRange(uint64(cursor), uint64(cursor+new_len))

			// Try to reparse within the child.
			if _range, ok := try_reparse(
				text,
				replaced,
				replacement_len,
				option.Some(node_kind),
				child,
				cursor,
			).Get(); ok {
				if child.len() != new_len {
					panic(fmt.Errorf("length mismatch; expected %v, got %v", child.len(), new_len))
				}
				new_desc := child.descendants()
				node.update_parent(prev_len, new_len, prev_desc, new_desc)
				return option.Some(_range)
			}

			// If the child is a block, try to reparse the block.
			if child.kind().is_block() {
				if newborn, ok := reparse_block(text, new_range).Get(); ok {
					if err := node.replace_children(ranges.NewRange(uint64(i), uint64(i+1)), []*SyntaxNode{newborn}); err != nil {
						return option.Some(new_range)
					}
				}
			}
		}

		// Does the child overlap with the edit?
		if overlaps(prev_range, replaced) {
			overlap.Start = min(overlap.Start, uint64(i))
			overlap.End = uint64(i + 1)
		}

		// Is the child beyond the edit?
		if replaced.End < uint64(cursor) {
			break
		}

		cursor += child.len()
	}

	// Try to reparse a range of markup expressions within markup. This is only
	// possible if the markup is top-level or contained in a block, not if it is
	// contained in things like headings or lists because too much can go wrong
	// with indent and line breaks.
	if overlap.IsEmpty() {
		return option.None[ranges.Range]()
	}
	if node.kind() != SyntaxKindMarkup {
		return option.None[ranges.Range]()
	}
	if kind, ok := parent_kind.Get(); !ok || ok && kind == SyntaxKindContentBlock {
		return option.None[ranges.Range]()
	}

	children := node.children()

	// Reparse a segment. Retries until it works, taking exponentially more
	// children into account.
	expansion := uint(1)
	for {
		// Add slack in both directions.
		start := saturating_sub(uint(overlap.Start), max(expansion, 2))
		end := min(uint(overlap.End)+expansion, uint(len(children)))

		// Expand to the left.
		for start > 0 && expand(children[start]) {
			start--
		}

		// Expand to the right.
		for end < uint(len(children)) && expand(children[end]) {
			end++
		}

		// Also take hash.
		if start > 0 && children[start-1].kind() == SyntaxKindHash {
			start--
		}

		// Synthesize what `at_start` and `nesting` would be at the start of the
		// reparse.
		prefix_len := uint(0)
		nesting := uint(0)
		at_start := true
		for _, child := range children[:start] {
			prefix_len += child.len()
			next_at_start(child, &at_start)
			next_nesting(child, &nesting)
		}

		// Determine what `at_start` will have to be at the end of the reparse.
		prev_len := uint(0)
		prev_at_start_after := at_start
		prev_nesting_after := nesting
		for _, child := range children[start:end] {
			prev_len += child.len()
			next_at_start(child, &prev_at_start_after)
			next_nesting(child, &prev_nesting_after)
		}

		// Determine the range in the new text that we want to reparse.
		shifted := offset + prefix_len
		new_len := prev_len + replacement_len - uint(replaced.Len())
		new_range := ranges.NewRange(uint64(shifted), uint64(shifted+new_len))
		at_end := end == uint(len(children))

		// Reparse!
		newborns, ok := reparse_markup(
			text,
			new_range,
			&at_start,
			&nesting,
			!parent_kind.IsPresent(),
		)
		if ok {
			// If more children follow, at_start must match its previous value.
			// Similarly, if we children follow or we not top-level the nesting
			// must match its previous value.
			if (at_end || at_start == prev_at_start_after) && ((at_end && !parent_kind.IsPresent()) || nesting == prev_nesting_after) {
				if err := node.replace_children(ranges.NewRange(uint64(start), uint64(end)), newborns); err == nil {
					return option.Some(new_range)
				}
			}
		}

		// If it didn't even work with all children, we give up.
		if start == 0 && at_end {
			break
		}

		// Exponential expansion to both sides.
		expansion *= 2
	}

	return option.None[ranges.Range]()
}

// Whether the inner range is fully contained in the outer one (no touching).
func includes(outer ranges.Range, inner ranges.Range) bool {
	return outer.Start < inner.Start && outer.End > inner.End
}

// Whether the first and second range overlap or touch.
func overlaps(first ranges.Range, second ranges.Range) bool {
	if first.Start <= second.Start && second.Start <= first.End {
		return true
	}
	if second.Start <= first.Start && first.Start <= second.End {
		return true
	}
	return false
}

// Whether the selection should be expanded beyond a node of this kind.
func expand(node *SyntaxNode) bool {
	kind := node.kind()
	if kind.is_trivia() {
		return true
	}
	if kind.is_error() {
		return true
	}
	if kind == SyntaxKindSemicolon {
		return true
	}
	switch node.text() {
	case "/", ":":
		return true
	}
	return false
}

// Whether `at_start` would still be true after this node given the
// previous value of the property.
func next_at_start(node *SyntaxNode, at_start *bool) {
	kind := node.kind()
	if kind.is_trivia() {
		if kind == SyntaxKindParbreak || (kind == SyntaxKindSpace && any_(node.text(), is_newline)) {
			*at_start = true
		}
	} else {
		*at_start = false
	}
}

// Update `nesting` based on the node.
func next_nesting(node *SyntaxNode, nesting *uint) {
	if node.kind() == SyntaxKindText {
		switch node.text() {
		case "[":
			*nesting++
		case "]":
			if *nesting > 0 {
				*nesting--
			}
		}
	}
}

func saturating_sub(a, b uint) uint {
	c := a - b
	if c > a {
		return 0
	}
	return c
}

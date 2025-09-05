package syntax

import (
	"reflect"
	"testing"

	"github.com/mewmew/typast/internal/ranges"
	"github.com/mewmew/typast/internal/vector"
)

type RangeTag struct {
	r   ranges.Range
	tag Tag
}

func test(t *testing.T, text string, want []RangeTag) {
	var tags vector.Vector[RangeTag]
	root := parse(text)
	//PrintRoot(root)
	highlight_tree(&tags, NewLinkedNode(root))
	got := []RangeTag(tags)
	if !reflect.DeepEqual(want, got) {
		t.Errorf("tag ranges mismatch; expected %v, got %v", want, got)
	}
}

func highlight_tree(tags *vector.Vector[RangeTag], node *LinkedNode) {
	if tag, ok := highlight(node).Get(); ok {
		tags.Push(RangeTag{r: node._range(), tag: tag})
	}

	for _, child := range node.children().Children() {
		highlight_tree(tags, child)
	}
}

func TestHighlighting(t *testing.T) {
	test(
		t,
		"= *AB*",
		[]RangeTag{
			{r: ranges.NewRange(0, 6), tag: Tag_Heading},
			{r: ranges.NewRange(2, 6), tag: Tag_Strong},
		},
	)

	test(t,
		"#f(x + 1)",
		[]RangeTag{
			{r: ranges.NewRange(0, 1), tag: Tag_Function},
			{r: ranges.NewRange(1, 2), tag: Tag_Function},
			{r: ranges.NewRange(2, 3), tag: Tag_Punctuation},
			{r: ranges.NewRange(5, 6), tag: Tag_Operator},
			{r: ranges.NewRange(7, 8), tag: Tag_Number},
			{r: ranges.NewRange(8, 9), tag: Tag_Punctuation},
		},
	)

	test(t,
		"#let f(x) = x",
		[]RangeTag{
			{r: ranges.NewRange(0, 1), tag: Tag_Keyword},
			{r: ranges.NewRange(1, 4), tag: Tag_Keyword},
			{r: ranges.NewRange(5, 6), tag: Tag_Function},
			{r: ranges.NewRange(6, 7), tag: Tag_Punctuation},
			{r: ranges.NewRange(8, 9), tag: Tag_Punctuation},
			{r: ranges.NewRange(10, 11), tag: Tag_Operator},
		},
	)
}

package syntax

/*
func TestLinkedNode() {
	source := Source_detached("#set text(12pt, red)")

	// Find "text" with Before.
	let node = LinkedNode::new(source.root()).leaf_at(7, Side::Before).unwrap();
	assert_eq!(node.offset(), 5);
	assert_eq!(node.text(), "text");

	// Find "text" with After.
	let node = LinkedNode::new(source.root()).leaf_at(7, Side::After).unwrap();
	assert_eq!(node.offset(), 5);
	assert_eq!(node.text(), "text");

	// Go back to "#set". Skips the space.
	let prev = node.prev_sibling().unwrap();
	assert_eq!(prev.offset(), 1);
	assert_eq!(prev.text(), "set");
}

func TestLinkedNodeNonTriviaLeaf() {
	source := Source_detached("#set fun(12pt, red)")
	let leaf = LinkedNode::new(source.root()).leaf_at(6, Side::Before).unwrap();
	let prev = leaf.prev_leaf().unwrap();
	assert_eq!(leaf.text(), "fun");
	assert_eq!(prev.text(), "set");

	// Check position 9 with Before.
	let source = Source::detached("#let x = 10");
	let leaf = LinkedNode::new(source.root()).leaf_at(9, Side::Before).unwrap();
	let prev = leaf.prev_leaf().unwrap();
	let next = leaf.next_leaf().unwrap();
	assert_eq!(prev.text(), "=");
	assert_eq!(leaf.text(), " ");
	assert_eq!(next.text(), "10");

	// Check position 9 with After.
	let source = Source::detached("#let x = 10");
	let leaf = LinkedNode::new(source.root()).leaf_at(9, Side::After).unwrap();
	let prev = leaf.prev_leaf().unwrap();
	assert!(leaf.next_leaf().is_none());
	assert_eq!(prev.text(), "=");
	assert_eq!(leaf.text(), "10");
}
*/

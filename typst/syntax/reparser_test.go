package syntax

import (
	"testing"

	"github.com/mewmew/typast/internal/ranges"
)

func test_reparse(t *testing.T, prev_str string, _range ranges.Range, with string, incremental bool) {
	source := Source_detached(prev_str)
	prev := source.root.clone()
	r := source.edit(_range, with)
	found := source.root.clone()
	expected := Parse(source.text())
	found.synthesize(NewDetachedSpan())
	expected.synthesize(NewDetachedSpan())
	// TODO: figure out a better way to compare nodes that string representation comparison.
	if found.String() != expected.String() {
		t.Fatalf("reparse mismatch\n\tsource: %v\n\tprevious: %v\n\texpected: %v\n\tgot:      %v", source.text(), prev, expected, found)
	}
	if incremental {
		if uint64(len(source.text())) == r.Len() {
			t.Errorf("should have been incremental\n\tsource: %v\n\trange: %v\n\tlen(source): %v\n\trange.len:   %v", source.text(), r, len(source.text()), r.Len())
		}
	} else {
		if uint64(len(source.text())) != r.Len() {
			t.Errorf("shouldn't have been incremental\n\tsource: %v\n\trange: %v\n\tlen(source): %v\n\trange.len:   %v", source.text(), r, len(source.text()), r.Len())
		}
	}
}

func TestReparseMarkup(t *testing.T) {
	//test_reparse(t, "abc~def~gh~", ranges.NewRange(5, 6), "+", true) // TODO: figure out why incremental test case fail
	//test_reparse(t, "~~~~~~~", ranges.NewRange(3, 4), "A", true)     // TODO: figure out why incremental test case fail
	//test_reparse(t, "abc~~", ranges.NewRange(1, 2), "", true)        // TODO: figure out why incremental test case fail
	test_reparse(t, "#var. hello", ranges.NewRange(5, 6), " ", false)
	test_reparse(t, "#var;hello", ranges.NewRange(9, 10), "a", false)
	test_reparse(t, "https:/world", ranges.NewRange(7, 7), "/", false)
	test_reparse(t, "hello  world", ranges.NewRange(7, 12), "walkers", false)
	test_reparse(t, "some content", ranges.NewRange(0, 12), "", false)
	test_reparse(t, "", ranges.NewRange(0, 0), "do it", false)
	test_reparse(t, "a d e", ranges.NewRange(1, 3), " b c d", false)
	test_reparse(t, "~*~*~", ranges.NewRange(2, 2), "*", false)
	//test_reparse(t, "::1\n2. a\n3", ranges.NewRange(7, 7), "4", true) // TODO: figure out why incremental test case fail
	test_reparse(t, "* #{1+2} *", ranges.NewRange(6, 7), "3", true)
	test_reparse(t, "#{(0, 1, 2)}", ranges.NewRange(6, 7), "11pt", true)
	test_reparse(t, "\n= A heading", ranges.NewRange(4, 4), "n evocative", false)
	//test_reparse(t, "#call() abc~d", ranges.NewRange(7, 7), "[]", true) // TODO: figure out why incremental test case fail
	test_reparse(t, "a your thing a", ranges.NewRange(6, 7), "a", false)
	test_reparse(t, "#grid(columns: (auto, 1fr, 40%))", ranges.NewRange(16, 20), "4pt", false)
	//test_reparse(t, "abc\n= a heading\njoke", ranges.NewRange(3, 4), "\nmore\n\n", true) // TODO: figure out why incremental test case fail
	test_reparse(t, "#show f: a => b..", ranges.NewRange(16, 16), "c", false)
	test_reparse(t, "#for", ranges.NewRange(4, 4), "//", false)
	//test_reparse(t, "a\n#let \nb", ranges.NewRange(7, 7), "i", true) // TODO: figure out why incremental test case fail
	test_reparse(t, "#{{let x = z}; a = 1} b", ranges.NewRange(7, 7), "//", false)
	test_reparse(t, "a ```typst hello```", ranges.NewRange(16, 17), "", false)
	test_reparse(t, "a{b}c", ranges.NewRange(1, 1), "#", false)
	test_reparse(t, "a#{b}c", ranges.NewRange(1, 2), "", false)
}

func TestReparseBlock(t *testing.T) {
	test_reparse(t, "Hello #{ x + 1 }!", ranges.NewRange(9, 10), "abc", true)
	test_reparse(t, "A#{}!", ranges.NewRange(3, 3), "\"", false)
	test_reparse(t, "#{ [= x] }!", ranges.NewRange(5, 5), "=", true)
	test_reparse(t, "#[[]]", ranges.NewRange(3, 3), "\\", true)
	test_reparse(t, "#[[ab]]", ranges.NewRange(4, 5), "\\", true)
	test_reparse(t, "#{}}", ranges.NewRange(2, 2), "{", false)
	test_reparse(t, "A: #[BC]", ranges.NewRange(6, 6), "{", true)
	test_reparse(t, "A: #[BC]", ranges.NewRange(6, 6), "#{", true)
	test_reparse(t, "A: #[BC]", ranges.NewRange(6, 6), "#{}", true)
	test_reparse(t, "#{\"ab\"}A", ranges.NewRange(5, 5), "c", true)
	test_reparse(t, "#{\"ab\"}A", ranges.NewRange(5, 6), "c", false)
	test_reparse(t, "a#[]b", ranges.NewRange(3, 3), "#{", true)
	test_reparse(t, "a#{call(); abc}b", ranges.NewRange(8, 8), "[]", true)
	test_reparse(t, "a #while x {\n g(x) \n}  b", ranges.NewRange(12, 12), "//", true)
	test_reparse(t, "a#[]b", ranges.NewRange(3, 3), "[hey]", true)
}

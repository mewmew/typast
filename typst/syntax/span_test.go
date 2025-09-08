package syntax

import (
	"math"
	"testing"

	"github.com/mewmew/typast/internal/ranges"
)

func TestSpanDetached(t *testing.T) {
	span := NewDetachedSpan()
	if !span.IsDetached() {
		t.Errorf("expected span to be detached")
	}
	if id, ok := span.ID(); ok {
		t.Errorf("expected span FileId to be None, got Some(%v)", id)
	}
	if r, ok := span.RawRange(); ok {
		t.Errorf("expected span range to be None, got Some(%v)", r)
	}
}

func TestSpanNumberEncoding(t *testing.T) {
	want_id := FileIDFromUint16(5)
	span, _ := NewSpanFromNumber(want_id, 10)
	if got_id, ok := span.ID(); ok {
		if want_id != got_id {
			t.Errorf("span FileId mismatch; expected %v, got %v", want_id, got_id)
		}
	} else {
		t.Errorf("expected valid span FileId, got None")
	}
	const want_number = 10
	got_number := span.Number()
	if want_number != got_number {
		t.Errorf("span number mismatch; expected %v, got %v", want_number, got_number)
	}
	if r, ok := span.RawRange(); ok {
		t.Errorf("expected span range to be None, got Some(%v)", r)
	}
}

func TestSpanRangeEncoding(t *testing.T) {
	want_id := FileIDFromUint16(uint16(math.MaxUint16))
	roundtrip := func(want_range ranges.Range) {
		span := NewSpanFromRange(want_id, want_range)
		if got_id, ok := span.ID(); ok {
			if want_id != got_id {
				t.Errorf("span FileId mismatch; expected %v, got %v", want_id, got_id)
			}
		} else {
			t.Errorf("expected valid span FileId, got None")
		}
		if got_range, ok := span.RawRange(); ok {
			if want_range != got_range {
				t.Errorf("span range mismatch; expected %v, got %v", want_range, got_range)
			}
		} else {
			t.Errorf("expected valid span range, got None")
		}
	}

	roundtrip(ranges.NewRange(0, 0))
	roundtrip(ranges.NewRange(177, 233))
	roundtrip(ranges.NewRange(0, 8388607))
	roundtrip(ranges.NewRange(8388606, 8388607))
}

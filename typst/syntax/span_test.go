package syntax

import (
	"math"
	"testing"

	"github.com/mewmew/typast/internal/ranges"
)

func TestSpanDetached(t *testing.T) {
	span := Span_detached()
	if !span.is_detached() {
		t.Errorf("expected span to be detached")
	}
	if id, ok := span.id().Get(); ok {
		t.Errorf("expected span FileId to be None, got Some(%v)", id)
	}
	if _range, ok := span._range().Get(); ok {
		t.Errorf("expected span range to be None, got Some(%v)", _range)
	}
}

func TestSpanNumberEncoding(t *testing.T) {
	want_id := FileIDFromUint16(5)
	span := Span_from_number(want_id, 10).MustGet()
	if got_id, ok := span.id().Get(); ok {
		if want_id != got_id {
			t.Errorf("span FileId mismatch; expected %v, got %v", want_id, got_id)
		}
	} else {
		t.Errorf("expected valid span FileId, got None")
	}
	const want_number = 10
	got_number := span.number()
	if want_number != got_number {
		t.Errorf("span number mismatch; expected %v, got %v", want_number, got_number)
	}
	if _range, ok := span._range().Get(); ok {
		t.Errorf("expected span range to be None, got Some(%v)", _range)
	}
}

func TestSpanRangeEncoding(t *testing.T) {
	want_id := FileIDFromUint16(uint16(math.MaxUint16))
	roundtrip := func(want_range ranges.Range) {
		span := Span_from_range(want_id, want_range)
		if got_id, ok := span.id().Get(); ok {
			if want_id != got_id {
				t.Errorf("span FileId mismatch; expected %v, got %v", want_id, got_id)
			}
		} else {
			t.Errorf("expected valid span FileId, got None")
		}
		if got_range, ok := span._range().Get(); ok {
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

package ranges

type Range struct {
	Start uint64
	End   uint64
}

func NewRange(start, end uint64) Range {
	return Range{
		Start: start,
		End:   end,
	}
}

func (r *Range) IsEmpty() bool {
	// TODO: double-check how to handle special case (used by reparser where
	// Start=Max and End=0)
	if r.Start > r.End {
		return true // special case
	}
	return r.Len() == 0
}

func (r *Range) Len() uint64 {
	return r.End - r.Start
}

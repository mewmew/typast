package vector

import (
	"slices"

	"github.com/mewmew/typast/internal/ranges"
)

type Vector[T any] []T

func (v *Vector[T]) Push(elem T) {
	*v = append(*v, elem)
}

func (v *Vector[T]) Drain(r ranges.Range) []T {
	start := int(r.Start)
	end := int(r.End)
	src := (*v)[start:end]
	n := len(src)
	dst := make([]T, n)
	copy(dst, src)
	*v = slices.Delete(*v, start, end)
	return dst
}

func (v *Vector[T]) Insert(index int, elem T) {
	*v = slices.Insert(*v, index, elem)
}

func (v *Vector[T]) Truncate(stop int) {
	*v = slices.Delete(*v, stop, len(*v))
}

func (v *Vector[T]) ExtendFromSlice(s []T) {
	*v = append(*v, s...)
}

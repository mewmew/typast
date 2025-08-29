// ref: https://github.com/golang-ds/stack (license: BSD-3-Clause)

// package stack implements a generic stack using a slice as the underlying data structure.
package stack

import "fmt"

type SliceStack[T any] struct {
	data []T
}

// New constructs and returns an empty slice-stack.
// time-complexity: O(1)
func New[T any]() SliceStack[T] {
	return SliceStack[T]{data: []T{}}
}

// Push adds an element on top of the stack.
// time-complexity: amortized O(1)
func (s *SliceStack[T]) Push(data T) {
	s.data = append(s.data, data)
}

// Pop removes and returns the top element of the stack. It returns false if the stack was empty.
// time-complexity: O(1)
func (s *SliceStack[T]) Pop() (val T, ok bool) {
	if s.IsEmpty() {
		return
	}

	val = s.data[len(s.data)-1]

	s.data = s.data[:len(s.data)-1]

	return val, true
}

// Top returns the top element of the stack. It returns false if the stack was empty.
// time-complexity: O(1)
func (s *SliceStack[T]) Top() (val T, ok bool) {
	if s.IsEmpty() {
		return
	}

	return s.data[len(s.data)-1], true
}

// Size returns the number of the elements in the stack.
// time-complexity: O(1)
func (s *SliceStack[T]) Size() int {
	return len(s.data)
}

// IsEmpty returns true if the stack doesn't have any elements.
// time-complexity: O(1)
func (s *SliceStack[T]) IsEmpty() bool {
	return s.Size() == 0
}

// String returns the string representation of the stack.
// time-complexity: O(n)
func (s *SliceStack[T]) String() string {
	return fmt.Sprint(s.data)
}

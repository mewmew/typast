package stack

import (
	"testing"

	"github.com/stretchr/testify/assert"
)

func TestPush(t *testing.T) {
	stack := New[int]()

	stack.Push(1)

	str := stack.String()
	assert.Equal(t, "[1]", str)

	stack.Push(2)
	stack.Push(3)

	str = stack.String()
	assert.Equal(t, "[1 2 3]", str)
}

func TestPop(t *testing.T) {
	stack := New[int]()

	top, ok := stack.Pop()
	assert.False(t, ok)
	assert.Equal(t, 0, top)

	stack.Push(1)
	stack.Push(2)
	stack.Push(3)

	top, ok = stack.Pop()
	assert.True(t, ok)
	assert.Equal(t, 3, top)

	str := stack.String()
	assert.Equal(t, "[1 2]", str)
}

func TestTop(t *testing.T) {
	stack := New[int]()

	top, ok := stack.Top()
	assert.False(t, ok)
	assert.Equal(t, 0, top)

	stack.Push(1)
	stack.Push(2)
	stack.Push(3)

	top, ok = stack.Top()
	assert.True(t, ok)
	assert.Equal(t, 3, top)

	str := stack.String()
	assert.Equal(t, "[1 2 3]", str)
}

func TestSize(t *testing.T) {
	stack := New[int]()

	size := stack.Size()
	assert.Equal(t, 0, size)

	stack.Push(1)
	stack.Push(2)
	stack.Push(3)

	size = stack.Size()
	assert.Equal(t, 3, size)
}

func TestIsEmpty(t *testing.T) {
	stack := New[int]()

	assert.True(t, stack.IsEmpty())

	stack.Push(1)
	stack.Push(2)
	stack.Push(3)

	assert.False(t, stack.IsEmpty())
}

func TestString(t *testing.T) {
	stack := New[int]()

	str := stack.String()
	assert.Equal(t, "[]", str)

	stack.Push(1)
	stack.Push(2)
	stack.Push(3)

	str = stack.String()
	assert.Equal(t, "[1 2 3]", str)
}

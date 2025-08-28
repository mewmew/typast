package option

// Option represents an optional value.
type Option[T any] struct {
	value *T
}

// Some creates an Option with a value.
func Some[T any](value T) Option[T] {
	return Option[T]{value: &value}
}

// None creates an empty Option.
func None[T any]() Option[T] {
	return Option[T]{value: nil}
}

// IsPresent checks if the Option contains a value.
func (o Option[T]) IsPresent() bool {
	return o.value != nil
}

// GetMust returns the value if present, otherwise panics.
func (o Option[T]) MustGet() T {
	if !o.IsPresent() {
		panic("attempted to get a None value")
	}
	return *o.value
}

// Get returns the value if present.
func (o Option[T]) Get() (T, bool) {
	if !o.IsPresent() {
		var zero T
		return zero, false
	}
	return *o.value, true
}

// Clone returns a copy of the Option.
func (o Option[T]) Clone() Option[T] {
	if v, ok := o.Get(); ok {
		return Some(v)
	}
	return None[T]()
}

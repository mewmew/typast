// File and package management.

package syntax

import (
	"fmt"
	"strings"
	"sync"

	"github.com/mewmew/typast/internal/option"
)

// The global package-path interner.
var INTERNER = &Interner{
	to_id:   make(map[Pair]FileId),
	from_id: nil,
}

// A package-path interner.
type Interner struct {
	to_id   map[Pair]FileId
	from_id []Pair // actually maps from id-1 to Pair (since id is NonZero).

	sync.RWMutex
}

// An interned pair of a package specification and a path.
type Pair struct {
	spec  option.Option[*PackageSpec]
	vpath *VirtualPath
}

// --- [ FileId ] --------------------------------------------------------------

// Identifies a file in a project or package.
//
// This type is globally interned and thus cheap to copy, compare, and hash.
type FileId uint16 // NonZeroU16

// Create a new interned file specification.
//
// The path must start with a `/` or this function will panic.
// Note that the path is normalized before interning.
//
// new
func NewFileId(spec option.Option[*PackageSpec], vpath *VirtualPath) FileId {
	// Try to find an existing entry that we can reuse.
	//
	// We could check with just a read lock, but if the pair is not yet
	// present, we would then need to recheck after acquiring a write lock,
	// which is probably not worth it.
	pair := Pair{
		spec:  spec,
		vpath: vpath,
	}
	INTERNER.Lock() // write lock
	defer INTERNER.Unlock()
	if id, ok := INTERNER.to_id[pair]; ok {
		return id
	}

	// Create a new entry forever by leaking the pair. We can't leak more
	// than 2^16 pair (and typically will leak a lot less), so its not a
	// big deal.
	id := push_pair(pair)
	return id
}

// pre-condition: mutex of INTERNER must be write-locked.
func push_pair(pair Pair) FileId {
	num := uint16(len(INTERNER.from_id)) + 1
	if num == 0 {
		panic("out of file ids") // uint16 overflow
	}
	id := FileId(num)
	if _, ok := pair.spec.Get(); ok {
		INTERNER.to_id[pair] = id // only add pair to to_id map if spec exist (i.e. not "fake" file specification)
	}
	INTERNER.from_id = append(INTERNER.from_id, pair)
	return id
}

// Create a new unique ("fake") file specification, which is not
// accessible by vpath.
//
// Caution: the ID returned by this method is the *only* identifier of the
// file, constructing a file ID with a path will *not* reuse the ID even
// if the path is the same. This method should only be used for generating
// "virtual" file ids such as content read from stdin.
func new_fake(vpath *VirtualPath) FileId {
	INTERNER.Lock() // write lock
	defer INTERNER.Unlock()
	pair := Pair{
		spec:  option.None[*PackageSpec](),
		vpath: vpath,
	}
	id := push_pair(pair)
	return id
}

// The package the file resides in, if any.
//
// package
func (id FileId) spec() option.Option[*PackageSpec] {
	pair := id.pair()
	return pair.spec
}

// The absolute and normalized path to the file _within_ the project or
// package.
func (id FileId) vpath() *VirtualPath {
	pair := id.pair()
	return pair.vpath
}

// Resolve a file location relative to this file.
func (id FileId) join(path string) FileId {
	return NewFileId(id.spec().Clone(), id.vpath().join(path))
}

// The same file location, but with a different extension.
func (id FileId) with_extension(extension string) FileId {
	return NewFileId(id.spec().Clone(), id.vpath().with_extension(extension))
}

// Construct from a raw number.
//
// Should only be used with numbers retrieved via
// [`into_raw`](Self::into_raw). Misuse may results in panics, but no
// unsafety.
//
// NonZeroU16
func from_raw(v uint16) FileId {
	return FileId(v)
}

// Extract the raw underlying number.
//
// NonZeroU16
func (id FileId) into_raw() uint16 {
	return uint16(id)
}

// Get the static pair.
func (id FileId) pair() Pair {
	INTERNER.RLock() // read lock
	defer INTERNER.RUnlock()
	return INTERNER.from_id[id-1] // NOTE: from_id actually maps from id-1 to Pair (since id is NonZero).
}

func (id FileId) String() string {
	buf := &strings.Builder{}
	vpath := id.vpath()
	if spec, ok := id.spec().Get(); ok {
		fmt.Fprintf(buf, "%v", spec)
	}
	fmt.Fprintf(buf, "%v", vpath)
	return buf.String()
}

// --- [/ FileId ] -------------------------------------------------------------

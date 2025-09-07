// File and package management.

package syntax

import (
	"fmt"
	"strings"
	"sync"

	"github.com/mewmew/typast/internal/option"
)

// interner is the global package-path interner.
var interner = &Interner{
	toID:   make(map[Pair]FileID),
	fromID: nil,
}

// Interner is a package-path interner.
type Interner struct {
	toID   map[Pair]FileID
	fromID []Pair // maps from id-1 to Pair (since id is NonZero).

	sync.RWMutex
}

// Pair is an interned pair of a package specification and a path.
type Pair struct {
	Spec  option.Option[*PackageSpec]
	VPath *VirtualPath
}

// FileID identifies a file in a project or package.
//
// This type is globally interned and thus cheap to copy, compare, and hash.
type FileID uint16 // NonZeroU16

// NewFileID creates a new interned file specification.
//
// The path must start with a `/` or this function will panic. The path is
// normalized before interning.
func NewFileID(spec option.Option[*PackageSpec], vpath *VirtualPath) FileID {
	pair := Pair{Spec: spec, VPath: vpath}
	interner.Lock()
	defer interner.Unlock()
	if id, ok := interner.toID[pair]; ok {
		return id
	}
	return pushPair(pair)
}

// pushPair creates a new entry forever.
//
// pre-condition: interner must be write-locked.
func pushPair(pair Pair) FileID {
	num := uint16(len(interner.fromID)) + 1
	if num == 0 {
		panic("out of file ids") // uint16 overflow
	}
	id := FileID(num)
	if _, ok := pair.Spec.Get(); ok {
		// only add pair to map if spec exists (i.e. not a "fake" file
		// specification)
		interner.toID[pair] = id
	}
	interner.fromID = append(interner.fromID, pair)
	return id
}

// NewFakeFileID creates a new unique ("fake") file specification, which is not
// accessible by vpath.
//
// Caution: the ID returned is the *only* identifier of the file. Constructing a
// file ID with a path will not reuse it, even if the path is the same. This
// method should only be used for generating "virtual" file IDs such as content
// read from stdin.
func NewFakeFileID(vpath *VirtualPath) FileID {
	interner.Lock()
	defer interner.Unlock()
	pair := Pair{Spec: option.None[*PackageSpec](), VPath: vpath}
	return pushPair(pair)
}

// Spec returns the package the file resides in, if any.
func (id FileID) Spec() option.Option[*PackageSpec] {
	pair := id.pair()
	return pair.Spec
}

// VPath returns the absolute and normalized path to the file within the project
// or package.
func (id FileID) VPath() *VirtualPath {
	pair := id.pair()
	return pair.VPath
}

// Join resolves a file location relative to this file.
func (id FileID) Join(path string) FileID {
	return NewFileID(id.Spec().Clone(), id.VPath().Join(path))
}

// WithExtension returns the same file location but with a different extension.
func (id FileID) WithExtension(extension string) FileID {
	return NewFileID(id.Spec().Clone(), id.VPath().WithExtension(extension))
}

// FileIDFromUint16 constructs a FileID from a raw number.
//
// Should only be used with numbers retrieved via Uint16. Misuse may result in
// panics.
func FileIDFromUint16(v uint16) FileID {
	return FileID(v)
}

// Uint16 extracts the raw underlying number.
func (id FileID) Uint16() uint16 {
	return uint16(id)
}

// pair retrieves the interned Pair for this FileID.
func (id FileID) pair() Pair {
	interner.RLock()
	defer interner.RUnlock()
	return interner.fromID[id-1]
}

// String returns a string representation of the FileID.
func (id FileID) String() string {
	out := &strings.Builder{}
	vpath := id.VPath()
	if spec, ok := id.Spec().Get(); ok {
		fmt.Fprintf(out, "%v", spec)
	}
	fmt.Fprintf(out, "%v", vpath)
	return out.String()
}

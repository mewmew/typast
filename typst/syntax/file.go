// File and package management.

package syntax

import "github.com/mewmew/typast/internal/option"

// Identifies a file in a project or package.
//
// This type is globally interned and thus cheap to copy, compare, and hash.
type FileId struct {
	spec option.Option[*PackageSpec]
	path *VirtualPath
}

// --- [ FileId ] --------------------------------------------------------------

// Create a new interned file specification.
//
// The path must start with a `/` or this function will panic.
// Note that the path is normalized before interning.
//
// new
func NewFileId(spec option.Option[*PackageSpec], path *VirtualPath) *FileId {
	return &FileId{
		spec: spec,
		path: path,
	}
}

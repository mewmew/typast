package syntax

import "path/filepath"

// VirtualPath is an absolute path in the virtual file system of a project or
// package.
type VirtualPath struct {
	Path string
}

// NewVirtualPath creates a new virtual path.
//
// Even if it doesn't start with `/` or `\`, it is still interpreted as
// starting from the root.
func NewVirtualPath(path string) *VirtualPath {
	return &VirtualPath{
		Path: path,
	}
}

// Join resolves a path relative to this virtual path.
func (vpath *VirtualPath) Join(path string) *VirtualPath {
	dir := filepath.Dir(vpath.Path)
	new_path := filepath.Join(dir, path)
	return NewVirtualPath(new_path)
}

// WithExtension returns the path, but with a different extension.
func (vpath *VirtualPath) WithExtension(extension string) *VirtualPath {
	ext := filepath.Ext(vpath.Path)
	name := vpath.Path[:len(vpath.Path)-len(ext)]
	new_path := name + extension
	return NewVirtualPath(new_path)
}

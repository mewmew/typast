package syntax

import "path/filepath"

// An absolute path in the virtual file system of a project or package.
type VirtualPath struct {
	path string
}

// Create a new virtual path.
//
// Even if it doesn't start with `/` or `\`, it is still interpreted as
// starting from the root.
//
// new
func NewVirtualPath(path string) *VirtualPath {
	return &VirtualPath{
		path: path,
	}
}

// Resolve a path relative to this virtual path.
func (vpath *VirtualPath) Join(path string) *VirtualPath {
	dir := filepath.Dir(vpath.path)
	new_path := filepath.Join(dir, path)
	return NewVirtualPath(new_path)
}

// The same path, but with a different extension.
func (vpath *VirtualPath) WithExtension(extension string) *VirtualPath {
	ext := filepath.Ext(vpath.path)
	name := vpath.path[:len(vpath.path)-len(ext)]
	new_path := name + extension
	return NewVirtualPath(new_path)
}

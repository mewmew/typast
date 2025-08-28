package syntax

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
func (vpath *VirtualPath) join(path string) *VirtualPath {
	dir := filepath.Dir(vpath.path)
	return filepath.Join(dir, path)
}

// The same path, but with a different extension.
func (vpath *VirtualPath) with_extension(extension string) *VirtualPath {
	ext := filepath.Ext(vpath.path)
	name := vpath.path[:len(vpath.path)-len(ext)]
	new_path := name + extension
	return NewVirtualPath(new_path)
}

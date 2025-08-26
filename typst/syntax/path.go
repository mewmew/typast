package syntax

// An absolute path in the virtual file system of a project or package.
type VirtualPath struct {
	PathBuf string
}

// Create a new virtual path.
//
// Even if it doesn't start with `/` or `\`, it is still interpreted as
// starting from the root.
//
// new
func NewVirtualPath(path string) *VirtualPath {
	return &VirtualPath{
		PathBuf: path,
	}
}

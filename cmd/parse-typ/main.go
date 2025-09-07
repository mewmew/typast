// parse-typ parses and prints the CST (Concrete Syntax Tree) of Typst files.
package main

import (
	"flag"
	"fmt"
	"log"
	"os"

	"github.com/mewmew/typast/typst/syntax"
	"github.com/pkg/errors"
)

func usage() {
	fmt.Fprintln(os.Stderr, "Usage: parse-typ [OPTION]... FILE.typ...")
	flag.PrintDefaults()
}

func main() {
	flag.Parse()
	for _, typPath := range flag.Args() {
		if err := parse(typPath); err != nil {
			log.Fatalf("%+v", err)
		}
	}
}

func parse(typPath string) error {
	// parse input Typst file.
	buf, err := os.ReadFile(typPath)
	if err != nil {
		return errors.WithStack(err)
	}
	text := string(buf)
	rootNode := syntax.Parse(text)
	// print CST.
	syntax.PrintRoot(rootNode)
	return nil
}

// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

//TODO trip test suite
//TODO Enlarge constants

//go:generate tangle tex.web
//go:generate cp -v tex.pool rtl.go assets/
//go:generate assets
//go:generate stringer -output stringer.go -linecomment -type=ch
//go:generate gofmt -l -s -w .

// Command web2go is an attempt to mechanically translate tex.web to Go. (Work
// in progress.)
//
// This program is being developed and tested using a tex.web file annotated as
//
//	% Version 3.141592653 was similar but more extensive (January 2021)
//
// and will probably crash on any other version or modification of it.  It's a
// non-goal, at this stage, to handle anything else than this one file of this
// single version.
//
// Installation
//
// To install:
//
//     $ go install modernc.org/web2go
//
// Dependencies
//
// Some external programs might be used, depending on what extension the
// input-file has.
//
//	TANGLE(1)
//
//	NAME
//	       tangle - translate WEB to Pascal
//
//	SYNOPSIS
//	       tangle [options] webfile[.web] [changefile[.ch]]
//
// Used to convert a .web, like tex.web to a .p (Pascal) file. Tangle binary is
// part of the Debian package 'texlive'.
//
// Invocation
//
// To run the command:
//
//	$ web2go [options] input-file
//
// The input file
//
// A .web file that will be processed by tangle to produce a .p file that will
// be the input of the transpiler.
//
// Options
//
// Flags that adjust program behavior
//
// 	-o output-file
//
// Set the Go output file name. Existing files will be overwritten
// without asking.
//
//	-p file-name
//
// When processing a .web file, keep the resulting .p file in file-name.
// Existing files will be overwritten without asking.
//
//	-pkg pkg-name
//
// When producing a library (see below) set the package name to pkg-name. Has
// no effect otherwise.
//
//	-lib
//
// Produce a library instead of a command.
//
//	-e
//
// Show all errors, if any.
//
// References
//
// Referenced to from elsewhere:
//
//	[0]: Kathleen Jensen, Niklaus Wirth: Pascal User Manual and Report, Fourth Edition.
//		ISBN-13: 978-0-387-97649-5 e-ISBN: 978-1-4612-4450-9
package main // import "modernc.org/web2go"

import (
	"bufio"
	"bytes"
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"runtime/debug"
	"strings"
)

var (
	goIdents = map[string]string{}
)

func goIdent(s string) (t string) {
	if t := goIdents[s]; t != "" {
		return t
	}

	defer func() { goIdents[s] = t }()

	if strings.ContainsRune(s, '_') {
		a := strings.Split(s, "_")
		for i, v := range a[1:] {
			a[i+1] = strings.ToUpper(string(rune(v[0]))) + v[1:]
		}
		t := strings.Join(a, "")
		return t
	}

	switch s {
	case "break":
		return "break1"
	case "error":
		return "error1"
	case "package":
		return "package1"
	default:
		return s
	}
}

func capitalize(s string) string { return strings.ToUpper(s[:1]) + s[1:] }

func fatalf(stack bool, s string, args ...interface{}) {
	if stack {
		fmt.Fprintf(os.Stderr, "%s\n", debug.Stack())
	}
	fmt.Fprintln(os.Stderr, strings.TrimSpace(fmt.Sprintf(s, args...)))
	os.Exit(1)
}

func fatal(stack bool, args ...interface{}) {
	if stack {
		fmt.Fprintf(os.Stderr, "%s\n", debug.Stack())
	}
	fmt.Fprintln(os.Stderr, strings.TrimSpace(fmt.Sprint(args...)))
	os.Exit(1)
}

func main() {
	task := newTask(os.Args)
	flag.BoolVar(&task.e, "e", false, "show all errors")
	flag.BoolVar(&task.lib, "lib", false, "produce an importable package instead of a command")
	flag.BoolVar(&task.stack, "stack", false, "show dying stack traces")
	flag.StringVar(&task.o, "o", "", ".go output file")
	flag.StringVar(&task.p, "p", "", ".p output file")
	flag.StringVar(&task.pkgName, "pkg", "tex", "name the output package, used only with -lib")
	flag.Parse()
	if flag.NArg() == 0 {
		fatal(task.stack, "missing input file argument")
	}

	switch flag.NArg() {
	case 0:
		fatal(task.stack, "missing input file argument")
	case 2:
		task.changeFile = flag.Arg(1)
		fallthrough
	case 1:
		task.in = flag.Arg(0)
	default:
		fatal(task.stack, "at most two input files expected")
	}

	if !task.lib {
		task.pkgName = "main"
	}
	if task.o == "" {
		fatal(task.stack, "missing output file argument")
	}

	if err := task.main(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

type task struct {
	args         []string
	changeFile   string
	cleanup      []func()
	copyright    []string
	in           string // tex.web, for example
	o            string // -o
	p            string // -p
	pkg          string // -pkg
	pkgName      string
	progTypeName string
	rcvrName     string
	tempDir      string

	e     bool // -e
	lib   bool // -lib
	stack bool // -stack
}

func newTask(args []string) *task {
	return &task{
		args:         args,
		pkgName:      "tex",
		progTypeName: "tex",
		rcvrName:     "tex",
	}
}

func (t *task) main() error {
	defer func() {
		for _, v := range t.cleanup {
			v()
		}
	}()

	if t.tempDir == "" {
		tempDir, err := ioutil.TempDir("", "web2go-")
		if err != nil {
			return err
		}

		t.tempDir = tempDir
		t.cleanup = append(t.cleanup, func() { os.RemoveAll(t.tempDir) })
	}

	if b, err := t.web2p(); err != nil {
		return fmt.Errorf("%s\n%v", b, err)
	}

	b, err := ioutil.ReadFile(t.p)
	if err != nil {
		return err
	}

	program, err := parse(t, b, t.p)
	if err != nil {
		return err
	}

	return newProject(t).main(program)
}

func (t *task) web2p() ([]byte, error) {
	const bin = "tangle"
	tangle, err := exec.LookPath(bin)
	if err != nil {
		return nil, err
	}

	b, err := ioutil.ReadFile(t.in)
	if err != nil {
		return nil, err
	}

	t.copyright = t.extractCopyright(b)
	f := filepath.Join(t.tempDir, t.in)
	if err := ioutil.WriteFile(f, b, 0660); err != nil {
		return nil, err
	}

	args := []string{"-underline", t.in}
	if t.changeFile != "" {
		args = append(args, t.changeFile)
	}
	if b, err = exec.Command(tangle, args...).CombinedOutput(); err != nil {
		return b, err
	}

	switch p := t.in[:len(t.in)-len(".web")] + ".p"; {
	case t.p == "":
		t.p = p
		t.cleanup = append(t.cleanup, func() { os.Remove(p) })
	default:
		raw, err := ioutil.ReadFile(p)
		if err != nil {
			return b, fmt.Errorf("could not read tangled output file %q: %w", p, err)
		}
		if err = ioutil.WriteFile(t.p, raw, 0666); err != nil {
			return b, fmt.Errorf("could not write tangled output file %q: %w", t.p, err)
		}
	}

	return b, nil
}

func (t *task) extractCopyright(b []byte) (r []string) {
	const maxLines = 100
	s := bufio.NewScanner(bytes.NewReader(b))
	line := 1
	for s.Scan() {
		const tag = "% "
		t := s.Text()
		perc := strings.HasPrefix(t, tag)
		if t != "" && !perc {
			break
		}

		if perc {
			t = t[len(tag):]
		}
		r = append(r, t)
		line++
		if line > maxLines {
			break
		}
	}
	for len(r) != 0 && r[0] == "" {
		r = r[1:]
	}
	for len(r) != 0 && r[len(r)-1] == "" {
		r = r[:len(r)-1]
	}
	if err := s.Err(); err != nil {
		r = append(r, fmt.Sprintf("ERROR: %s", err))
	}
	return r
}

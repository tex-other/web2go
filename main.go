// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command web2go is an attempt to mechanically translate tex.web to Go. (Work
// in progress.)
//
// This program is being developed and tested using a tex.web file annotated as
//
//	% Version 3.141592653 was similar but more extensive (January 2021)
//
// and will probably crash on any other version or modification of it.  It's a
// non-goal to handle anything else than this one file with this single
// version.
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
// Used to convert .web to .p.
//
//	ptop(1)
//
//	NAME
//	       ptop - The FPC Pascal configurable source beautifier.
//
//	       Origin probably Pascal-TO-Pascal.
//
//	SYNOPSIS
//	       ptop[-v][-iindent][-bbufsize][-coptsfile]<infile><outfile>
//
// Used to optionally convert .p to .pas.
//
// Invocation
//
// To run the command:
//
//	$ web2go [options] input-file
//
// Input files
//
// A .web file will be processed by tangle to produce a .p file that will be
// the input of the transpiler. If the -ptop option is given, the .p file will
// by formatted by ptop to produce a .pas file which will be the transpiler
// input.
//
// A .p file will be the input file of the transpiler. If the -ptop option is
// given, the .p file will by formatted by ptop to produce a .pas file which
// will be the transpiler input.
//
// A .pas file is the input of the transpiler.
//
// Options
//
// Flags that adjust program begavior
//
// 	-o output-file
//
// Explicitly set the Go output file name. Existing files will be overwritten
// without asking.
//
//	-p file-name
//
// When processing a .web file, keep the resulting .p file in file-name.
// Existing files will be overwritten without asking.
//
//	-pas file-name
//
// When formatting a .p file, keep the resulting .pas file in file-name.
// Existing files will be overwritten without asking.
//
//	-ptop
//
// If the input-file is .web or .p, format the .p file using ptop to produce a
// .pas file that will be the input of the transpiler.  Requires ptop to be
// installed.
//
// References
//
// Referenced to from elsewhere:
//
//	[0]: Kathleen Jensen, Niklaus Wirth: Pascal User Manual and Report, Fourth Edition.
//		ISBN-13: 978-0-387-97649-5 e-ISBN: 978-1-4612-4450-9
package main // import "modernc.org/web2go"

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"runtime/debug"
	"strings"
)

func fatalf(s string, args ...interface{}) {
	fmt.Fprintf(os.Stderr, "%s\n", debug.Stack())
	fmt.Fprintln(os.Stderr, strings.TrimSpace(fmt.Sprintf(s, args...)))
	os.Exit(1)
}

func fatal(args ...interface{}) {
	fmt.Fprintf(os.Stderr, "%s\n", debug.Stack())
	fmt.Fprintln(os.Stderr, strings.TrimSpace(fmt.Sprint(args...)))
	os.Exit(1)
}

func origin(skip int) string {
	pc, fn, fl, _ := runtime.Caller(skip)
	fn = filepath.Base(fn)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
	}
	return fmt.Sprintf("%s:%d:%s", fn, fl, fns)
}

func todo(s string, args ...interface{}) string { //TODO-
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	pc, fn, fl, _ := runtime.Caller(1)
	f := runtime.FuncForPC(pc)
	var fns string
	if f != nil {
		fns = f.Name()
		if x := strings.LastIndex(fns, "."); x > 0 {
			fns = fns[x+1:]
		}
	}
	r := fmt.Sprintf("%s:%d:%s: TODOTODO %s", fn, fl, fns, s) //TODOOK
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

func trc(s string, args ...interface{}) string { //TODO-
	switch {
	case s == "":
		s = fmt.Sprintf(strings.Repeat("%v ", len(args)), args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	_, fn, fl, _ := runtime.Caller(1)
	r := fmt.Sprintf("%s:%d: TRC %s", fn, fl, s)
	fmt.Fprintf(os.Stdout, "%s\n", r)
	os.Stdout.Sync()
	return r
}

func main() {
	task := newTask(os.Args)
	flag.BoolVar(&task.ptop, "ptop", false, "format .p to .pas")
	flag.StringVar(&task.o, "o", "", ".go output file")
	flag.StringVar(&task.p, "p", "", ".p output file")
	flag.StringVar(&task.pas, "pas", "", ".pas output file")
	flag.Parse()
	if flag.NArg() == 0 {
		fatal("missing input file argument")
	}

	if flag.NArg() > 1 {
		fatal("only one input file expected")
	}

	task.in = flag.Arg(0)
	if err := task.main(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}

type task struct {
	args    []string
	cleanup []func()
	in      string
	o       string
	p       string
	pas     string
	tempDir string

	ptop bool
}

func newTask(args []string) *task {
	return &task{
		args: args,
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

	panic(todo(""))
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

	f := filepath.Join(t.tempDir, t.in)
	if err := ioutil.WriteFile(f, b, 0660); err != nil {
		return nil, err
	}

	if b, err = exec.Command(tangle, t.in).CombinedOutput(); err != nil {
		return b, err
	}

	switch p := t.in[:len(t.in)-len(".web")] + ".p"; {
	case t.p == "":
		t.p = p
		t.cleanup = append(t.cleanup, func() { os.Remove(p) })
	default:
		if err := os.Rename(p, t.p); err != nil {
			return b, err
		}
	}

	return b, nil
}

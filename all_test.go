// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"path"
	"path/filepath"
	"runtime"
	"runtime/debug"
	"strings"
	"testing"
)

func caller(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	_, fn, fl, _ := runtime.Caller(2)
	fmt.Fprintf(os.Stderr, "# caller: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	_, fn, fl, _ = runtime.Caller(1)
	fmt.Fprintf(os.Stderr, "# \tcallee: %s:%d: ", path.Base(fn), fl)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func dbg(s string, va ...interface{}) {
	if s == "" {
		s = strings.Repeat("%v ", len(va))
	}
	pc, fn, fl, _ := runtime.Caller(1)
	f := runtime.FuncForPC(pc)
	fmt.Fprintf(os.Stderr, "# dbg %s:%d:%s: ", path.Base(fn), fl, f.Name())
	fmt.Fprintf(os.Stderr, s, va...)
	fmt.Fprintln(os.Stderr)
	os.Stderr.Sync()
}

func TODO(...interface{}) string { //TODOOK
	_, fn, fl, _ := runtime.Caller(1)
	return fmt.Sprintf("# TODO: %s:%d:\n", path.Base(fn), fl) //TODOOK
}

func stack() []byte { return debug.Stack() }

func use(...interface{}) {}

func init() {
	use(caller, dbg, TODO, stack) //TODOOK
}

// ----------------------------------------------------------------------------
var tempDir string

func TestMain(m *testing.M) {
	flag.Parse()
	os.Exit(testMain(m))
}

func testMain(m *testing.M) int {
	var err error
	tempDir, err = ioutil.TempDir("", "web2go-test-")
	if err != nil {
		panic(err)
	}

	defer os.RemoveAll(tempDir)

	return m.Run()
}

func TestScanner(t *testing.T) {
	task := newTask(os.Args)
	task.in = "tex.web"
	var err error
	task.changeFile, err = filepath.Abs("changefile.ch")
	if err != nil {
		t.Fatal(err)
	}

	task.tempDir = tempDir
	task.p = filepath.Join(task.tempDir, "tex.p")
	if b, err := task.web2p(); err != nil {
		t.Fatalf("%s\n%v", b, err)
	}

	b, err := ioutil.ReadFile(task.p)
	if err != nil {
		t.Fatal(err)
	}

	s, err := newScanner(b, "tex.p")
	if err != nil {
		t.Fatal(err)
	}

	var toks int
	for {
		tok := s.scan()
		if tok.ch < 0 {
			break
		}

		toks++
	}
	if err := s.errList(); err != nil {
		t.Fatal(err)
	}

	t.Logf("toks: %v", toks)
}

func TestParser(t *testing.T) {
	task := newTask(os.Args)
	task.in = "tex.web"
	var err error
	task.changeFile, err = filepath.Abs("changefile.ch")
	if err != nil {
		t.Fatal(err)
	}

	task.tempDir = tempDir
	task.p = filepath.Join(task.tempDir, "tex.p")
	if b, err := task.web2p(); err != nil {
		t.Fatalf("%s\n%v", b, err)
	}

	b, err := ioutil.ReadFile(task.p)
	if err != nil {
		t.Fatal(err)
	}

	program, err := parse(b, "tex.p")
	if err != nil {
		t.Fatalf("could not parse: %+v", err)
	}

	if program == nil {
		t.Fatal("empty result but no error")
	}
}

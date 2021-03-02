// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"flag"
	"fmt"
	"io/ioutil"
	"os"
	"os/exec"
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
	const changeFile = "changefile.ch"
	task := newTask(os.Args)
	task.tempDir = tempDir
	task.in = "tex.web"
	pth := filepath.Join(tempDir, changeFile)
	if err := ioutil.WriteFile(pth, []byte(assets["/"+changeFile]), 0660); err != nil {
		t.Fatal(err)
	}

	task.changeFile = pth
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
}

func TestParser(t *testing.T) {
	const changeFile = "changefile.ch"
	task := newTask(os.Args)
	task.tempDir = tempDir
	task.in = "tex.web"
	pth := filepath.Join(tempDir, changeFile)
	if err := ioutil.WriteFile(pth, []byte(assets["/"+changeFile]), 0660); err != nil {
		t.Fatal(err)
	}

	task.changeFile = pth
	task.p = filepath.Join(task.tempDir, "tex.p")
	if b, err := task.web2p(); err != nil {
		t.Fatalf("%s\n%v", b, err)
	}

	b, err := ioutil.ReadFile(task.p)
	if err != nil {
		t.Fatal(err)
	}

	program, err := parse(task, b, "tex.p")
	if err != nil {
		t.Fatalf("could not parse: %+v", err)
	}

	if program == nil {
		t.Fatal("empty result but no error")
	}
}

func TestGenerator(t *testing.T) {
	const changeFile = "changefile.ch"
	task := newTask(os.Args)
	task.tempDir = tempDir
	task.in = "tex.web"
	task.o = filepath.Join(tempDir, "tex.go")
	pth := filepath.Join(tempDir, changeFile)
	if err := ioutil.WriteFile(pth, []byte(assets["/"+changeFile]), 0660); err != nil {
		t.Fatal(err)
	}

	task.changeFile = pth
	task.p = filepath.Join(task.tempDir, "tex.p")
	if b, err := task.web2p(); err != nil {
		t.Fatalf("%s\n%v", b, err)
	}

	b, err := ioutil.ReadFile(task.p)
	if err != nil {
		t.Fatal(err)
	}

	program, err := parse(task, b, "tex.p")
	if err != nil {
		t.Fatalf("could not parse: %+v", err)
	}

	if program == nil {
		t.Fatal("empty result but no error")
	}

	func() {
		project := newProject(task)
		defer func() {
			if e := recover(); e != nil {
				project.err(nil, "%s\n%v", stack(), e)
				err = fmt.Errorf("%s", strings.Join(project.errs, "\n"))
			}
		}()

		err = project.main(program)
	}()
	if testing.Verbose() {
		b, _ = ioutil.ReadFile(task.o)
		fmt.Printf("%s\n", b)
	}
	if err != nil {
		t.Fatal(err)
	}

	if b, err = exec.Command("go", "build", "-o", os.DevNull, task.o).CombinedOutput(); err != nil {
		t.Fatalf("%s\n%v", b, err)
	}
}

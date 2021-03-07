// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"bytes"
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

	"modernc.org/ccgo/v3/lib"
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
	task.tempDir = tempDir
	task.in = "tex.web"
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
	task := newTask(os.Args)
	task.tempDir = tempDir
	task.in = "tex.web"
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
	task := newTask(os.Args)
	task.tempDir = tempDir
	task.in = "tex.web"
	task.o = filepath.Join(tempDir, "tex.go")
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

// tripman.pdf, Appendix A: How to test TEX.
func TestTrip(t *testing.T) {
	tempDir, err := ioutil.TempDir("", "go-test-web2go-")
	if err != nil {
		t.Fatal(err)
	}

	defer os.RemoveAll(tempDir)

	// 0. Let’s assume that you have a tape containing TRIP.TEX, TRIP.PL,
	// TRIPIN.LOG, TRIP.LOG, TRIP.TYP, and TRIP.FOT, as in Appendices B, C, D, E,
	// F, and G. Furthermore, let’s suppose that you have a working WEB system, and
	// that you have working programs TFtoPL, PLtoTF, DVItype, as described in the
	// TEXware report.

	var tftopl, pltotf, dvitype string
	for _, v := range []string{
		"trip.tex",
		"trip.pl",
		"tripin.log",
		"trip.log",
		"trip.typ",
		"trip.fot",
	} {
		if _, err := ccgo.CopyFile(filepath.Join(tempDir, v), filepath.Join("trip", v), nil); err != nil {
			t.Fatal(err)
		}
	}
	if tftopl, err = exec.LookPath("tftopl"); err != nil {
		t.Fatal(err)
	}

	if pltotf, err = exec.LookPath("pltotf"); err != nil {
		t.Fatal(err)
	}

	if dvitype, err = exec.LookPath("dvitype"); err != nil {
		t.Fatal(err)
	}

	// 1. Use PLtoTF to convert TRIP.PL into TRIP.TFM. Then use TFtoPL to convert
	// TRIP.TFM into TMP.PL.  Check that TMP.PL is identical to TRIP.PL (this is a
	// partial test of PLtoTF and TFtoPL). Install TRIP.TFM in the standard file
	// area for TEX font metric files.
	if _, err := ccgo.Shell(
		pltotf,
		filepath.Join(tempDir, "trip.pl"),
		filepath.Join(tempDir, "trip.tfm"),
	); err != nil {
		t.Fatal(err)
	}
	if _, err := ccgo.Shell(
		tftopl,
		filepath.Join(tempDir, "trip.tfm"),
		filepath.Join(tempDir, "tmp.pl"),
	); err != nil {
		t.Fatal(err)
	}

	g, err := ioutil.ReadFile(filepath.Join(tempDir, "tmp.pl"))
	if err != nil {
		t.Fatal(err)
	}

	e, err := ioutil.ReadFile(filepath.Join(tempDir, "trip.pl"))
	if err != nil {
		t.Fatal(err)
	}

	if !bytes.Equal(g, e) {
		t.Fatal("tmp.pl does not equal trip.pl")
	}

	if g, err = ioutil.ReadFile(filepath.Join(tempDir, "trip.tfm")); err != nil {
		t.Fatal(err)
	}

	if e, err = ioutil.ReadFile(filepath.Join("assets", "texfonts", "trip.tfm")); err != nil {
		t.Fatal(err)
	}

	if !bytes.Equal(g, e) {
		t.Fatal("trip.tfm does not equal the installed one")
	}

	// 2. Prepare a special version of INITEX. (This means that your WEB change
	// file should have init and tini defined to be null.) The stat and tats macros
	// should also be null, so that statistics are kept and other special features
	// are enabled. Set mem min and mem bot equal to 1, and set mem top and mem max
	// equal to 3000, for purposes of this test version. Also set error line = 64,
	// half error line = 32, and max print line = 72; these parameters affect many
	// of the lines of the test output, so your job will be much easier if you use
	// the same settings that were used to produce Appendix E. You probably should
	// also use the “normal” settings of other parameters found in TEX.WEB (e.g.,
	// stack size = 200, font max = 75, etc.), since these show up in a few lines
	// of the test output. Your test version should not change the default
	// definition of unprintable characters (§49 of the program).

	task := newTask([]string{"test-trip"})
	task.tempDir = tempDir
	task.in = "tex.web"
	task.changeFile = "trip.ch"
	task.o = filepath.Join(tempDir, "tex.go")
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

	project := newProject(task)
	if err = project.main(program); err != nil {
		t.Fatal(err)
	}

	_ = dvitype
}

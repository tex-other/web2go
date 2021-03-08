// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"bytes"
	"flag"
	"fmt"
	"io"
	"io/ioutil"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"runtime"
	"runtime/debug"
	"strings"
	"testing"
	"time"

	"github.com/pmezard/go-difflib/difflib"
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

	for _, v := range []string{
		"trip.tex",
		"trip.pl",
	} {
		if _, err := ccgo.CopyFile(filepath.Join(tempDir, v), filepath.Join("trip", v), nil); err != nil {
			t.Fatal(err)
		}
	}

	testDir, err := ccgo.AbsCwd()
	if err != nil {
		t.Fatal(err)
	}

	if err := ccgo.InDir(tempDir, func() error { return testTrip(testDir, tempDir) }); err != nil {
		t.Fatal(err)
	}
}

func testTrip(testDir, tempDir string) error {
	// 1. Use PLtoTF to convert TRIP.PL into TRIP.TFM. Then use TFtoPL to convert
	// TRIP.TFM into TMP.PL.  Check that TMP.PL is identical to TRIP.PL (this is a
	// partial test of PLtoTF and TFtoPL). Install TRIP.TFM in the standard file
	// area for TEX font metric files.

	pltotf, err := exec.LookPath("pltotf")
	if err != nil {
		return err
	}

	if _, err := ccgo.Shell(pltotf, "trip.pl", "trip.tfm"); err != nil {
		return err
	}

	tftopl, err := exec.LookPath("tftopl")
	if err != nil {
		return err
	}

	if _, err := ccgo.Shell(tftopl, "trip.tfm", "tmp.pl"); err != nil {
		return err
	}

	g, err := ioutil.ReadFile("tmp.pl")
	if err != nil {
		return err
	}

	e, err := ioutil.ReadFile("trip.pl")
	if err != nil {
		return err
	}

	if !bytes.Equal(g, e) {
		return fmt.Errorf("tmp.pl does not equal trip.pl")
	}

	if g, err = ioutil.ReadFile("trip.tfm"); err != nil {
		return err
	}

	if e, err = ioutil.ReadFile(filepath.Join(testDir, "assets", "texfonts", "trip.tfm")); err != nil {
		return err
	}

	if !bytes.Equal(g, e) {
		return fmt.Errorf("trip.tfm does not equal the installed one")
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

	if err := ccgo.InDir(testDir, func() error {
		task := newTask([]string{"test-trip"})
		task.tempDir = tempDir
		task.in = "tex.web"
		task.pkgName = "main"
		task.changeFile = filepath.Join("trip", "trip.ch")
		task.o = filepath.Join(tempDir, "tex.go")
		task.p = filepath.Join(task.tempDir, "tex.p")
		task.trip = true
		return task.main()
	}); err != nil {
		return err
	}

	// 3. Run the INITEX prepared in step 2. In response to the first ‘**’ prompt,
	// type carriage return (thus getting another ‘**’). Then type ‘\input trip’.
	// You should get an output that matches the file TRIPIN.LOG (Appendix D).
	// Don’t be alarmed by the error messages that you see, unless they are
	// different from those in Appendix D.

	cmd := exec.Command("go", "run", filepath.Join(tempDir, "tex.go"))
	stdin, err := cmd.StdinPipe()
	if err != nil {
		return err
	}

	stdout, err := cmd.StdoutPipe()
	if err != nil {
		return err
	}

	if err := cmd.Start(); err != nil {
		return err
	}

	rx := func(n int, timeout time.Duration, r io.Reader) (string, error) {
		ta := time.After(timeout)
		ch := make(chan error)
		b := make([]byte, n)
		go func() {
			_, err := io.ReadFull(r, b)
			ch <- err
		}()

		select {
		case err := <-ch:
			if err != nil {
				return "", err
			}

			return string(b), nil
		case <-ta:
			return "", fmt.Errorf("rx: timeout")
		}
	}

	rxLine := func(timeout time.Duration, r io.Reader) (string, error) {
		ta := time.After(timeout)
		var b strings.Builder
		for {
			s, err := rx(1, timeout, r)
			if err != nil {
				return "", err
			}

			if s == "\n" {
				return b.String(), nil
			}

			select {
			case <-ta:
				return b.String(), fmt.Errorf("rxLine: timeout")
			default:
				b.WriteString(s)
			}

		}
	}

	// "This is TeX, Version 3.141592653 (INITEX)"
	if _, err := rxLine(time.Second, stdout); err != nil {
		return err
	}

	prompt, err := rx(2, time.Second, stdout)
	if err != nil {
		return err
	}

	if g, e := prompt, "**"; g != e {
		return fmt.Errorf("got %q, expected %q", g, e)
	}

	if _, err = stdin.Write([]byte{'\n'}); err != nil {
		return err
	}

	// "Please type the name of your input file."
	if _, err = rxLine(time.Second, stdout); err != nil {
		return err
	}

	if prompt, err = rx(2, time.Second, stdout); err != nil {
		return err
	}

	if g, e := prompt, "**"; g != e {
		return fmt.Errorf("got %q, expected %q", g, e)
	}

	input := "\\input trip"
	if _, err = stdin.Write([]byte(input + "\n")); err != nil {
		return err
	}

	go func() {
		for {
			if _, err = rxLine(time.Second, stdout); err != nil {
				break
			}
		}
	}()

	if err = cmd.Wait(); err != nil {
		return err
	}

	got, err := ioutil.ReadFile("trip.log")
	if err != nil {
		return err
	}

	exp, err := ioutil.ReadFile(filepath.Join(testDir, "trip", "tripin.log"))
	if err != nil {
		return err
	}

	if !bytes.Equal(got, exp) {
		diff := difflib.UnifiedDiff{
			A:        difflib.SplitLines(string(exp)),
			B:        difflib.SplitLines(string(got)),
			FromFile: filepath.Join(testDir, "trip", "tripin.log"),
			ToFile:   filepath.Join(tempDir, "trip.log"),
			Context:  3,
		}
		text, _ := difflib.GetUnifiedDiffString(diff)
		return fmt.Errorf("\n%s", text)
	}

	// 4. Run INITEX again. This time type ‘ &trip trip ’. (The spaces in this
	// input help to check certain parts of TEX that aren’t otherwise used.) You
	// should get outputs TRIP.LOG, TRIP.DVI, and TRIPOS.TEX; there will also be an
	// empty file 8TERMINAL.TEX. Furthermore, your terminal should receive output
	// that matches TRIP.FOT (Appendix H). During the middle part of this test,
	// however, the terminal will not be getting output, because \batchmode is
	// being tested; don’t worry if nothing seems to be happening for a
	// while—nothing is supposed to.

	if err = os.Remove("trip.log"); err != nil {
		return err
	}

	cmd = exec.Command("go", "run", filepath.Join(tempDir, "tex.go"))
	if stdin, err = cmd.StdinPipe(); err != nil {
		return err
	}

	cmd.Stdin = bytes.NewBuffer([]byte(" &trip  trip \n"))
	if got, err = cmd.Output(); err != nil {
		return err
	}

	for _, v := range []string{
		"trip.log",
		"trip.dvi",
		"tripos.tex",
		"8terminal.tex",
	} {
		_, err := os.Stat(v)
		if err != nil {
			return fmt.Errorf("output file missing: %v", v)
		}
	}

	x := bytes.Index(got, []byte("(trip.tex ##"))
	got = got[x:]
	got = bytes.TrimSpace(got)
	if exp, err = ioutil.ReadFile(filepath.Join(testDir, "trip", "trip.fot")); err != nil {
		return err
	}

	x = bytes.Index(exp, []byte("(trip.tex ##"))
	exp = exp[x:]
	exp = bytes.TrimSpace(exp)
	if !bytes.Equal(got, exp) {
		diff := difflib.UnifiedDiff{
			A:        difflib.SplitLines(string(exp)),
			B:        difflib.SplitLines(string(got)),
			FromFile: filepath.Join(testDir, "trip", "trip.fot"),
			ToFile:   "terminal output",
			Context:  0,
		}
		text, _ := difflib.GetUnifiedDiffString(diff)
		return fmt.Errorf("\n%s", text)
	}

	// 5. Compare the TRIP.LOG file from step 4 with the “master” TRIP.LOG file of
	// step 0. (Let’s hope you put that master file in a safe place so that it
	// wouldn’t be clobbered.) There should be perfect agreement between these
	// files except in the following respects:
	//
	// a) The dates and possibly the file names will naturally be different.
	//
	// b) Glue settings in the displays of TEX boxes are subject to
	// system-dependent rounding, so slight deviations are permissible. However,
	// such deviations apply only to the ‘glue set’ values that appear at the end
	// of an \hbox or \vbox line; all other numbers should agree exactly, since
	// they are computed with integer arithmetic in a prescribed system-independent
	// manner.
	//
	// c) The amount of space in kerns that are marked “for accent” are, similarly,
	// subject to system- dependent rounding.
	//
	// d) If you had different values for stack size , buf size , etc., the
	// corresponding capacity values will be different when they are printed out at
	// the end.
	//
	// e) Help messages may be different; indeed, the author encourages non-English
	// help messages in versions of TEX for people who don’t understand English as
	// well as some other language.
	//
	// f) The total number and length of strings at the end may well be different.
	//
	// g) If your TEX uses a different memory allocation or packing scheme or DVI
	// output logic, the memory usage statistics may change.

	if got, err = ioutil.ReadFile("trip.log"); err != nil {
		return err
	}

	// b)
	// @@ -4796 +4796 @@
	// -..\hbox(8.0+2.0)x0.0, glue set 177.80537fil
	// +..\hbox(8.0+2.0)x0.0, glue set 177.80539fil
	got = bytes.Replace(
		got,
		[]byte("\\hbox(8.0+2.0)x0.0, glue set 177.80539fil"),
		[]byte("\\hbox(8.0+2.0)x0.0, glue set 177.80537fil"),
		1,
	)
	if exp, err = ioutil.ReadFile(filepath.Join(testDir, "trip", "trip.log")); err != nil {
		return err
	}

	if !bytes.Equal(got, exp) {
		diff := difflib.UnifiedDiff{
			A:        difflib.SplitLines(string(exp)),
			B:        difflib.SplitLines(string(got)),
			FromFile: filepath.Join(testDir, "trip", "trip.log"),
			ToFile:   filepath.Join(tempDir, "trip.log"),
			Context:  0,
		}
		text, _ := difflib.GetUnifiedDiffString(diff)
		return fmt.Errorf("\n%s", text)
	}

	// 6. Use DVItype to convert your file TRIP.DVI to a file TRIP.TYP. The
	// following options should be set when using DVItype:
	//
	// 	Output level = 2
	// 	Starting page = *.*.*.*.*.*.*.*.*.*
	// 	Number of pages = 1000000	(this is the default)
	// 	Resolution = 7227/100		(this is one point per pixel)
	// 	New magnification = 0		(this is the default)
	//
	// The resulting file should agree with the master TRIP.TYP file of step 0,
	// except that some of the values might be a little off due to floating-point
	// rounding discrepancies. Furthermore there may be differences between ‘right
	// ’ and ‘w ’ and ‘x ’ commands, and between ‘down ’ and ‘y ’ and ‘z ’; the key
	// thing is that all characters and rules and xxx ’s should be in almost the
	// same positions as specified in Appendix F. (If your DVI-writing routines
	// differ substantially from those in TEX.WEB, you may want to write a
	// DVIcompare program that detects any substantive differences between two
	// given DVI files. Such a routine would be of general use besides. On the
	// other hand, if you have set dvi buf size to 800, then your DVI file should
	// be virtually identical to the one supplied.)

	dvitype, err := exec.LookPath("dvitype")
	if err != nil {
		return err
	}

	if got, err = exec.Command(
		dvitype,
		"-output-level=2",
		"-page-start=*.*.*.*.*.*.*.*.*.*",
		"-max-pages=1000000",
		"-dpi=72.27",
		"-magnification=0",
		"trip.dvi",
	).Output(); err != nil {
		return err
	}

	if exp, err = ioutil.ReadFile(filepath.Join(testDir, "trip", "trip.typ")); err != nil {
		return err
	}

	// @@ -1 +1 @@
	// -This is DVItype, Version 3.6
	// +This is DVItype, Version 3.6 (TeX Live 2019/dev/Debian)
	x = bytes.IndexByte(got, '\n')
	got = got[x+1:]
	x = bytes.IndexByte(exp, '\n')
	exp = exp[x+1:]

	// @@ -1065 +1065 @@
	// -2458: right4 12176941
	// +2458: right4 12176942
	got = bytes.Replace(got, []byte("2458: right4 12176942"), []byte("2458: right4 12176941"), 1)
	if !bytes.Equal(got, exp) {
		diff := difflib.UnifiedDiff{
			A:        difflib.SplitLines(string(exp)),
			B:        difflib.SplitLines(string(got)),
			FromFile: filepath.Join(testDir, "trip", "trip.typ"),
			ToFile:   "dvitype output",
			Context:  0,
		}
		text, _ := difflib.GetUnifiedDiffString(diff)
		return fmt.Errorf("\n%s", text)
	}

	// 7. You might also wish to test TRIP with other versions of TEX (i.e., VIRTEX
	// or a production version with other fonts and macros preloaded). It should
	// work unless TEX’s primitives have been redefined.  However, this step isn’t
	// essential, since all the code of VIRTEX appears in INITEX; you probably
	// won’t catch any more errors this way, unless they would already become
	// obvious from normal use of the system.

	// Nothing to do here.

	return nil
}

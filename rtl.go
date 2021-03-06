// NOTE: This file (rtl.go) has a different license compared to all of the
// other Go files in this repository.

// ----------------------------------------------------------------------------
//
// No Copyright
//
// The person who associated a work with this deed has dedicated the work to
// the public domain by waiving all of his or her rights to the work worldwide
// under copyright law, including all related and neighboring rights, to the
// extent allowed by law.
//
// You can copy, modify, distribute and perform the work, even for commercial
// purposes, all without asking permission. See Other Information below.
//
// This license is acceptable for Free Cultural Works.
//
// Other Information
//
// In no way are the patent or trademark rights of any person affected by CC0,
// nor are the rights that other persons may have in the work or in how the
// work is used, such as publicity or privacy rights.
//
// Unless expressly stated otherwise, the person who associated a work with
// this deed makes no warranties about the work, and disclaims liability for
// all uses of the work, to the fullest extent permitted by applicable law.
//
// When using or citing the work, you should not imply endorsement by the
// author or the affirmer.
//
// ----------------------------------------------------------------------------
// Source of the above: https://creativecommons.org/publicdomain/zero/1.0/

package main

import (
	"bufio"
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"unsafe"
)

type memoryWord struct {
	variant float64
}

/* CUT HERE */

var (
	_ error = pasError(0)

	isMain bool
)

const (
	pasEndOfTeX pasError = -iota - 1
	pasFinalEnd
)

const (
	stdioDev      = "TTY:"
	modeNoIOPanic = "/O"
)

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

type pasError int

func (err pasError) Error() string { return fmt.Sprintf("%T(%[1]d)", err) }

func pasJumpOut() { panic(pasEndOfTeX) }

func abs(n float32) float32 { return float32(math.Abs(float64(n))) } //TODO check the Pascal definition.

func chr(i int32) byte { return byte(i) }

func iabs(n int32) int32 {
	if n >= 0 {
		return n
	}

	if n == math.MinInt32 {
		panic("overflow")
	}

	return -n
}

func odd(i int32) bool { return i&1 != 0 }

func round(n float32) int32 { return int32(math.Round(float64(n))) } //TODO check the Pascal definition.

func read(args ...interface{})    { args[0].(*pasFile).read(args[1:], false) }
func readLn(args ...interface{})  { args[0].(*pasFile).read(args[1:], true) }
func write(args ...interface{})   { args[0].(*pasFile).write(args[1:], false) }
func writeLn(args ...interface{}) { args[0].(*pasFile).write(args[1:], true) }

type pasFile struct {
	*ioFile
}

func break1(f *pasFile)                      { /* nop */ }
func breakIn(f *pasFile, b bool)             { /* nop */ }
func close(f *pasFile)                       { f.close() }
func eof(f *pasFile) bool                    { return f.ioFile.eof() }
func eoln(f *pasFile) bool                   { return f.ioFile.eoln() }
func erstat(f *pasFile) int32                { return f.erstat() }
func get(f *pasFile)                         { f.get() }
func put(f *pasFile)                         { f.put() }
func reset1(f *pasFile, name, mode string)   { reset(f, 1, name, mode) }
func reset4(f *pasFile, name, mode string)   { reset(f, 4, name, mode) }
func rewrite1(f *pasFile, name, mode string) { rewrite(f, 1, name, mode) }
func rewrite4(f *pasFile, name, mode string) { rewrite(f, 4, name, mode) }

func reset(f *pasFile, itemSize int, name, mode string) {
	name = strings.TrimRight(name, " ")
	panicOnError := !strings.Contains(mode, modeNoIOPanic)
	f.close()
	f.ioFile = nil
	if isMain && name == stdioDev {
		f.ioFile = &ioFile{
			itemSize:     itemSize,
			name:         name,
			panicOnError: panicOnError,
			r0:           os.Stdin,
			r:            bufio.NewReader(os.Stdin),
		}
		return
	}

	g, err := os.Open(name)
	if err != nil {
		if panicOnError {
			f.ioFile = &ioFile{
				err:          err,
				itemSize:     itemSize,
				name:         name,
				panicOnError: panicOnError,
			}
			return
		}
	}

	f.ioFile = &ioFile{
		itemSize:     itemSize,
		name:         name,
		panicOnError: panicOnError,
		r0:           g,
		r:            bufio.NewReader(g),
	}
}

func rewrite(f *pasFile, itemSize int, name, mode string) {
	name = strings.TrimRight(name, " ")
	panicOnError := !strings.Contains(mode, modeNoIOPanic)
	f.close()
	f.ioFile = nil
	if isMain && name == stdioDev {
		f.ioFile = &ioFile{
			itemSize:     itemSize,
			name:         os.Stdout.Name(),
			panicOnError: panicOnError,
			w:            os.Stdout,
		}
		return
	}

	g, err := os.Create(name)
	if err != nil {
		panic(todo(""))
	}

	f.ioFile = &ioFile{
		itemSize:     itemSize,
		name:         name,
		panicOnError: panicOnError,
		w:            g, //TODO bufio?
	}
}

func (f *pasFile) byte() byte {
	if f.itemSize != 1 {
		panic(todo("internal error: %v", f.itemSize))
	}

	if !f.componentMode {
		f.get()
	}

	return f.buf[0]
}

func (f *pasFile) memoryWord() memoryWord {
	if f.itemSize != 8 {
		panic(todo("internal error: %v", f.itemSize))
	}

	return *(*memoryWord)(unsafe.Pointer(&f.buf[0]))
}

func (f *pasFile) pMemoryWord() *memoryWord {
	if f.itemSize != 8 {
		panic(todo("internal error: %v", f.itemSize))
	}

	return (*memoryWord)(unsafe.Pointer(&f.buf[0]))
}

type ioFile struct {
	r0 *os.File
	r  *bufio.Reader
	w  io.Writer

	b1       [1]byte
	buf      [4]byte
	err      error
	itemSize int
	name     string

	componentMode bool
	panicOnError  bool
}

func (f *ioFile) close() {
	if f == nil {
		return
	}

	if x, ok := f.w.(io.Closer); ok {
		if f.err = x.Close(); f.err != nil && f.panicOnError {
			panic(fmt.Errorf("I/O error: %v", f.err))
		}

		f.w = nil
		return
	}

	if f.r != nil {
		if f.err = f.r0.Close(); f.err != nil && f.panicOnError {
			panic(fmt.Errorf("I/O error: %v", f.err))
		}

		f.r0 = nil
		f.r = nil
		return
	}
}

func (f *ioFile) read(args []interface{}, nl bool) {
	for len(args) != 0 {
		arg := args[0]
		args = args[1:]
		if w, ok := getWidth(&args); ok {
			panic(todo("", w))
		}

		switch x := arg.(type) {
		case *byte:
			var n int
			if n, f.err = f.r.Read(f.buf[:1]); n != 1 {
				panic(todo("%q %v %v", f.name, n, f.err))
			}

			*x = f.buf[0]
		default:
			panic(todo("%T", x))
		}
	}
	if !nl {
		return
	}

	for {
		var n int
		n, f.err = f.r.Read(f.b1[:])
		if n == 0 || f.b1[0] == '\n' {
			return
		}
	}
}

func (f *ioFile) eof() bool {
	if f.err == io.EOF {
		return true
	}

	var n int
	if n, f.err = f.r.Read(f.b1[:]); n == 0 || f.err == io.EOF {
		return true
	}

	f.r.UnreadByte()
	return false
}

func (f *ioFile) eoln() bool {
	if f.err == io.EOF {
		return true
	}

	if f.componentMode {
		return f.buf[0] == '\n'
	}

	var n int
	if n, f.err = f.r.Read(f.b1[:]); n == 0 || f.err == io.EOF {
		f.err = io.EOF
		return true
	}

	f.r.UnreadByte()
	return f.b1[0] == '\n'
}

func (f *ioFile) write(args []interface{}, nl bool) {
	for len(args) != 0 {
		arg := args[0]
		args = args[1:]
		if w, ok := getWidth(&args); ok {
			panic(todo("", w))
		}

		switch x := arg.(type) {
		case string:
			if _, f.err = f.w.Write([]byte(x)); f.err != nil && f.panicOnError {
				panic(fmt.Errorf("I/O error: %v", f.err))
			}
		case byte:
			f.buf[0] = x
			if _, f.err = f.w.Write(f.buf[:1]); f.err != nil && f.panicOnError {
				panic(fmt.Errorf("I/O error: %v", f.err))
			}
		default:
			panic(todo("%T", x))
		}
	}
	if nl {
		if _, f.err = f.w.Write([]byte("\n")); f.err != nil && f.panicOnError {
			panic(fmt.Errorf("I/O error: %v", f.err))
		}
	}
}

func (f *ioFile) get() {
	f.componentMode = true
	var n int
	if n, f.err = f.r.Read(f.buf[:f.itemSize]); n != f.itemSize && f.panicOnError {
		panic(fmt.Errorf("I/O error: %v", f.err))
	}
}

func (f *ioFile) put() {
	var n int
	if n, f.err = f.w.Write(f.buf[:f.itemSize]); n != f.itemSize && f.panicOnError {
		panic(fmt.Errorf("I/O error: %v", f.err))
	}
}

func (f *ioFile) erstat() int32 {
	if f.err == nil {
		return 0
	}

	return 1
}

type vaWidth int

func getWidth(args *[]interface{}) (int, bool) {
	s := *args
	if len(s) == 0 {
		return 0, false
	}

	x, ok := s[0].(vaWidth)
	if !ok {
		return 0, false
	}

	*args = s[1:]
	return int(x), true
}

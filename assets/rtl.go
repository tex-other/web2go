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
	"fmt"
	"io"
	"math"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"time"
	"unsafe"
)

type memoryWord struct {
	variant float32
}

/* CUT HERE */

var (
	_ error         = pasError(0)
	_ io.ReadCloser = readCloser{}

	isMain bool
)

const (
	pasEndOfTeX pasError = -iota - 1
	pasFinalEnd
)

const (
	modeNoIOPanic = "/O"
	stdioDev      = "TTY:"
	texArea       = "TeXinputs:"
	texFontArea   = "TeXfonts:"
	texPool       = "TeXformats:TEX.POOL"
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

func abs(n float32) float32       { return float32(math.Abs(float64(n))) }
func chr(i int32) byte            { return byte(i) }
func odd(i int32) bool            { return i&1 != 0 }
func pasSysDay() int32            { return int32(time.Now().Day()) }
func pasSysMonth() int32          { return int32(time.Now().Month()) }
func pasSysTime() int32           { return int32(time.Now().Hour()*60 + time.Now().Minute()) }
func pasSysYear() int32           { return int32(time.Now().Year()) }
func read(args ...interface{})    { args[0].(*pasFile).read(args[1:], false) }
func readLn(args ...interface{})  { args[0].(*pasFile).read(args[1:], true) }
func write(args ...interface{})   { args[0].(*pasFile).write(args[1:], false) }
func writeLn(args ...interface{}) { args[0].(*pasFile).write(args[1:], true) }

func iabs(n int32) int32 {
	if n >= 0 {
		return n
	}

	if n == math.MinInt32 {
		panic("overflow")
	}

	return -n
}

// [0] page 193
//
// round (r) yields a value such that if r >= 0 then round (r) =
// trunc(r + 0.5), and if r < 0 then round(r) =
// trunc (r - 0.5). It is an error if no such value exists.
func round(r float32) int32 {
	if r >= 0 {
		return int32(r + 0.5)
	}

	return int32(r - 0.5)
}

func setString(dst []byte, src string) {
	for i := range dst {
		dst[i] = ' '
	}
	copy(dst, src)
}

type pasFile struct {
	*ioFile
}

func (f *pasFile) byte() byte { return f.component[0] }

func (f *pasFile) memoryWord() memoryWord {
	if g, e := uintptr(f.componentSize), unsafe.Sizeof(memoryWord{}); g != e { //TODO-
		panic(todo("invalid component: got %v, expected %v (%q)", g, e, f.name))
	}

	return *(*memoryWord)(unsafe.Pointer(&f.component))
}

func (f *pasFile) pMemoryWord() *memoryWord {
	if g, e := uintptr(f.componentSize), unsafe.Sizeof(memoryWord{}); g != e { //TODO-
		panic(todo("invalid component: got %v, expected %v (%q)", g, e, f.name))
	}

	return (*memoryWord)(unsafe.Pointer(&f.component))
}

func break1(f *pasFile)                      { /* nop */ }
func breakIn(f *pasFile, b bool)             { /* nop */ }
func close(f *pasFile)                       { f.close() }
func eof(f *pasFile) bool                    { return f.ioFile.eof }
func eoln(f *pasFile) bool                   { return f.ioFile.eoln() }
func erstat(f *pasFile) int32                { return f.erstat }
func get(f *pasFile)                         { f.get() }
func put(f *pasFile)                         { f.put() }
func reset1(f *pasFile, name, mode string)   { reset(f, 1, name, mode) }
func reset4(f *pasFile, name, mode string)   { reset(f, 4, name, mode) }
func rewrite1(f *pasFile, name, mode string) { rewrite(f, 1, name, mode) }
func rewrite4(f *pasFile, name, mode string) { rewrite(f, 4, name, mode) }

type readCloser struct {
	io.Reader
}

func (r readCloser) Close() error { return nil }

// [0] page 87
//
// Reset (F) initiates inspection (reading) of F by placing the file at its
// beginning. If F is not empty, the value of the first component of F is
// assigned to F and eof (F) becomes false.
func reset(f *pasFile, componentSize int, name, mode string) {
	name = strings.TrimRight(name, " ")
	if !strings.Contains(mode, modeNoIOPanic) {
		panic(fmt.Errorf("unsupported file mode: %q (%q)", mode, name))
	}

	f.close()
	f.ioFile = nil
	if name == stdioDev {
		if isMain {
			f.ioFile = &ioFile{
				eof:           false,
				erstat:        0,
				componentSize: componentSize,
				name:          os.Stdin.Name(),
				in:            os.Stdin, //TODO bufio
			}
			return
		}

		panic(todo("")) //TODO -lib
	}

	g, err := os.Open(name)
ok:
	switch {
	case err != nil:
		switch {
		case name == texPool:
			f.ioFile = &ioFile{
				eof:           false,
				erstat:        0,
				componentSize: componentSize,
				name:          name,
				in:            readCloser{strings.NewReader(assets["/tex.pool"])},
			}
			break ok
		case strings.HasPrefix(name, texArea):
			if s, ok := assets["/texinputs/"+name[len(texArea):]]; ok {
				f.ioFile = &ioFile{
					eof:           false,
					erstat:        0,
					componentSize: componentSize,
					name:          name,
					in:            readCloser{strings.NewReader(s)},
				}
				break ok
			}
		case strings.HasPrefix(name, texFontArea):
			if s, ok := assets["/texfonts/"+name[len(texFontArea):]]; ok {
				f.ioFile = &ioFile{
					eof:           false,
					erstat:        0,
					componentSize: componentSize,
					name:          name,
					in:            readCloser{strings.NewReader(s)},
				}
				break ok
			}
		}

		f.ioFile = &ioFile{
			erstat:        1,
			componentSize: componentSize,
			name:          name,
		}
		return
	default:
		f.ioFile = &ioFile{
			eof:           false,
			erstat:        0,
			componentSize: componentSize,
			name:          name,
			in:            g, //TODO bufio
		}
	}

	if _, err := io.ReadFull(f.ioFile.in, f.ioFile.component[:f.ioFile.componentSize]); err != nil {
		f.ioFile.eof = true
		f.ioFile.erstat = 1
		f.ioFile.in.Close()
	}
}

// [0] page 88.
//
// Rewrite (F) initiates generation (writing) of the file F. The current value
// of F is replaced with the empty file. Eof(F) becomes true, and a new file
// may be written.
func rewrite(f *pasFile, componentSize int, name, mode string) {
	name = strings.TrimRight(name, " ")
	if !strings.Contains(mode, modeNoIOPanic) {
		panic(fmt.Errorf("unsupported file mode: %q", mode))
	}

	f.close()
	f.ioFile = nil
	if name == stdioDev {
		if isMain {
			f.ioFile = &ioFile{
				eof:           true,
				erstat:        0,
				componentSize: componentSize,
				name:          os.Stdout.Name(),
				out:           os.Stdout, //TODO bufio
			}
			return
		}

		panic(todo("")) // -lib
	}

	g, err := os.Create(name)
	if err != nil {
		f.ioFile = &ioFile{
			eof:           false,
			erstat:        1,
			componentSize: componentSize,
			name:          name,
		}
		return
	}

	f.ioFile = &ioFile{
		eof:           true,
		erstat:        0,
		componentSize: componentSize,
		name:          name,
		out:           g, //TODO bufio
	}
}

type ioFile struct {
	component     [unsafe.Sizeof(memoryWord{})]byte
	erstat        int32
	in            io.ReadCloser
	componentSize int
	name          string
	out           io.WriteCloser

	eof bool
	eol bool
}

func (f *ioFile) close() {
	if f == nil {
		return
	}

	if f.in != nil {
		if err := f.in.Close; err != nil {
			f.erstat = 1
		}
		f.eof = true
		f.in = nil
	}

	if f.out != nil {
		if err := f.out.Close; err != nil {
			f.erstat = 1
		}
		f.eof = false
		f.in = nil
	}
}

// [0] page 88
//
// Read (F, X) (for X, a variable) is equivalent to
//
// 	begin
// 		X := F^; Get(F)
// 	end
//
// Read (F, V1, ... , Vn) is equivalent to the statement
//
//	begin Read(F,V1); ... ;Read(F,Vn) end
func (f *ioFile) read(args []interface{}, nl bool) {
	f.eol = false
	for len(args) != 0 {
		arg := args[0]
		args = args[1:]
		if _, ok := getWidth(&args); ok {
			panic("internal error: read field width specifier not supported")
		}

		switch x := arg.(type) {
		case *byte:
			*x = f.component[0]
		default:
			panic(fmt.Errorf("unsupported read variable type: %T (%q)", x, f.name))
		}
		f.get()
	}
	if !nl {
		return
	}

	// [0] page 92
	//
	// ReadLn(F) skips to the beginning of the next line of the textfile F (F^
	// becomes the first character of the next line).
	for !f.eof && f.component[0] != '\n' {
		f.get()
	}
	if !f.eof {
		f.get()
	}
}

// [0] page 88
//
// Write(F, E) (for E, an expresion) is equivalent to
//
// 	begin
// 		F^ := E; Put(F)
// 	end
// Write (F, E1, ... , En) is equivalent to the statement
//
//	begin Write(F,E1); ... ; Write(F,En) end
func (f *ioFile) write(args []interface{}, nl bool) {
	for len(args) != 0 {
		arg := args[0]
		args = args[1:]
		w, ok := getWidth(&args)
		if _, ok2 := getWidth(&args); ok2 {
			panic("internal error: write fraction field width specifier not supported")
		}

		var err error
		switch x := arg.(type) {
		case string:
			_, err = f.out.Write([]byte(x))
		case byte:
			if ok {
				_, err = fmt.Fprintf(f.out, "%*d", w, x)
				break
			}

			f.component[0] = x
			f.put()
		case int32:
			if ok {
				_, err = fmt.Fprintf(f.out, "%*d", w, x)
				break
			}

			_, err = fmt.Fprint(f.out, x)
		case int:
			if ok {
				_, err = fmt.Fprintf(f.out, "%*d", w, x)
				break
			}

			_, err = fmt.Fprint(f.out, x)
		case uint16:
			if ok {
				_, err = fmt.Fprintf(f.out, "%*d", w, x)
				break
			}

			_, err = fmt.Fprint(f.out, x)
		default:
			panic(fmt.Errorf("unsupported write variable type: %T (%q)", x, f.name))
		}
		if err != nil {
			panic(fmt.Errorf("write I/O error: %v (%q)", err, f.name))
		}
	}
	// [0] page 92
	//
	// Writeln (F) terminates the current line of the textfile F.
	if nl {
		f.write([]interface{}{"\n"}, false)
	}
}

// [0] page 88
//
// Get(F) advances the file to the next component and assigns the value of this
// component to the buffer variable F^. If no next component exists, then
// eof(F) becomes true, and F^ becomes undefined. The effect of Get (F) is an
// error if eof(F) is true prior to its execution or if F is being generated.
func (f *ioFile) get() {
	if f.eof {
		panic(fmt.Errorf("get called at eof: %s", f.name))
	}

	f.eol = false
	if _, err := io.ReadFull(f.in, f.component[:f.componentSize]); err != nil {
		f.eof = true
		f.erstat = 1
		f.in.Close()
	}
}

// [0] page 88
//
// Put(F) appends the value of the buffer variable F^ to the file F. The effect
// is an error unless prior to execution the predicate eof(F) is true. eof(F)
// remains true, and F^ becomes undefined. Put(F) is an error if F is being
// inspected.
func (f *ioFile) put() {
	if !f.eof {
		panic(fmt.Errorf("put called not at eof: %s", f.name))
	}

	if _, err := f.out.Write(f.component[:f.componentSize]); err != nil {
		panic(fmt.Errorf("put I/O error: %v (%q)", err, f.name))
	}
}

// [0] page 92
//
// Eoln(F)
//
// a Boolean function indicating whether the end of the current line in the
// textfile F has been reached. (If true, F^ corresponds to the position of a
// line separator, but F^ is a blank.)
func (f *ioFile) eoln() bool {
	if f.eol || f.eof {
		return true
	}

	if f.component[0] != '\n' {
		return false
	}

	f.component[0] = ' '
	f.eol = true
	return true
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

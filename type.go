// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"go/token"
	"math"
	"strings"
	"unsafe"

	"modernc.org/cc/v3"
	"modernc.org/strutil"
)

var (
	_ typ = (*array)(nil)
	_ typ = (*boolean)(nil)
	_ typ = (*char)(nil)
	_ typ = (*file)(nil)
	_ typ = (*integer)(nil)
	_ typ = (*real)(nil)
	_ typ = (*record)(nil)
	_ typ = (*stringType)(nil)
	_ typ = (*subrange)(nil)

	aBoolean = &boolean{}
	aChar    = &char{}
	aInteger = &integer{}
	aReal    = &real{}

	stringTypes = map[int]*stringType{}
)

type typ interface {
	node
	goName() string
	setPacked()
	size() int
	cName() string
}

type noder struct{}

func (noder) Position() (r token.Position) { return r }

type char struct {
	noder
}

func (t *char) cName() string  { return "char" }
func (t *char) goName() string { return "byte" }
func (t *char) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }
func (t *char) size() int      { return int(unsafe.Sizeof(byte(0))) }

type integer struct {
	noder
}

func (t *integer) cName() string  { return "int" }
func (t *integer) goName() string { return "int32" }
func (t *integer) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }
func (t *integer) size() int      { return int(unsafe.Sizeof(int32(0))) }

type real struct {
	noder
}

func (t *real) cName() string  { return "double" }
func (t *real) goName() string { return "float64" }
func (t *real) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }
func (t *real) size() int      { return int(unsafe.Sizeof(float64(0))) }

type boolean struct {
	noder
}

func (t *boolean) cName() string  { return "char" }
func (t *boolean) goName() string { return "bool" }
func (t *boolean) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }
func (t *boolean) size() int      { return int(unsafe.Sizeof(byte(0))) }

type stringType struct {
	noder
	sz int
}

func stringTypeForSize(sz int) *stringType {
	if x := stringTypes[sz]; x != nil {
		return x
	}

	t := &stringType{sz: sz}
	stringTypes[sz] = t
	return t
}

func (t *stringType) cName() string  { return fmt.Sprintf("char[%d]", t.sz) }
func (t *stringType) goName() string { return fmt.Sprintf("[%d]byte", t.sz) }
func (t *stringType) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }
func (t *stringType) size() int      { return t.sz }

type subrange struct {
	noder
	lo, hi int
	sz     int
	cNm    string
	goNm   string
}

func newSubrange(lo, hi int) *subrange {
	r := &subrange{lo: lo, hi: hi}
	switch {
	case lo >= 0 && hi >= 0:
		switch {
		case hi <= math.MaxUint8:
			r.cNm = "char"
			r.goNm = "byte"
			r.sz = int(unsafe.Sizeof(byte(0)))
		case hi <= math.MaxUint16:
			r.cNm = "short"
			r.goNm = "uint16"
			r.sz = int(unsafe.Sizeof(uint16(0)))
		case hi <= math.MaxUint32:
			r.cNm = "unsigned"
			r.goNm = "uint32"
			r.sz = int(unsafe.Sizeof(uint32(0)))
		default:
			panic(todo("internal error: %v %v", lo, hi))
		}
	default:
		panic(todo("", lo, hi))
	}
	return r
}

func (t *subrange) cName() string  { return t.cNm }
func (t *subrange) goName() string { return t.goNm }
func (t *subrange) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }
func (t *subrange) size() int      { return t.sz }

type file struct {
	noder
	component typ

	packed bool
}

func newFile(component typ) *file { return &file{component: component} }

func (t *file) cName() string  { return "void*[2]" }
func (t *file) goName() string { return "io.ReadWriter" }
func (t *file) setPacked()     { t.packed = true }
func (t *file) size() int      { return int(unsafe.Sizeof(interface{}(nil))) }

type field struct {
	name string
	off  uintptr //TODO
	typ
}

type record struct {
	noder
	cNm    string
	ctyp   cc.Type
	fields []field
	sz     int

	packed     bool
	singleItem bool
}

func newRecord(goos, goarch string, fieldList *fieldList) (*record, error) {
	r := &record{}
	var b strings.Builder
	w := strutil.IndentFormatter(&b, "\t")
	fieldList.c(w)
	r.cNm = b.String()
	abi, err := cc.NewABI(goos, goarch)
	if err != nil {
		return r, err
	}

	cfg := &cc.Config{ABI: abi}
	ast, err := cc.Translate(cfg, nil, nil, []cc.Source{{Name: "record_layout", Value: r.cNm + " record;"}})
	if err != nil {
		return r, err
	}

	var d *cc.Declarator
	id := cc.String("record")
	for k := range ast.TLD {
		if k.Name() == id {
			d = k
			break
		}
	}
	if d == nil {
		return r, fmt.Errorf("internal error while computing record layout")
	}

	r.ctyp = d.Type()
	r.sz = int(r.ctyp.Size())
	return r, nil
}

func (t *record) cName() string { return t.cNm }

func (t *record) goName() string {
	//TODO panic(todo(""))
	return "TODO-record"
}

func (t *record) setPacked() { t.packed = true }
func (t *record) size() int  { return t.sz }

type array struct {
	noder
	dims []typ
	elem typ
	sz   int

	packed bool
}

func newArray(n node, dims []*typeNode, elem typ) *array {
	r := &array{elem: elem, sz: elem.size()}
	for _, v := range dims {
		r.dims = append(r.dims, v.typ)
		switch x := v.typ.(type) {
		case *char:
			r.sz *= 256
		case *subrange:
			r.sz *= (x.hi - x.lo + 1)
		default:
			panic(todo("%v: %T", n.Position(), x))
		}
	}
	return r
}

func (t *array) cName() string  { panic(todo("")) }
func (t *array) goName() string { panic(todo("")) }
func (t *array) setPacked()     { t.packed = true }
func (t *array) size() int      { return t.sz }

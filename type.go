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
	_ typ = (*pasString)(nil)
	_ typ = (*subrange)(nil)

	aBoolean = &boolean{}
	aChar    = &char{}
	aInteger = &integer{}
	aReal    = &real{}

	stringTypes = map[uintptr]*pasString{}

	idRecord = cc.String("record")
	idU      = cc.String("__u__")
)

type typ interface {
	node
	cType() string
	goType() string
	pasKind() string
	size() uintptr
}

type ordinal interface {
	cardinality() uintptr
}

type packer interface {
	setPacked()
}

type tagger interface {
	tag() string
}

type noder struct{}

func (noder) Position() (r token.Position) { return r }

type char struct {
	noder
}

func (t *char) cardinality() uintptr { return math.MaxUint8 + 1 }
func (t *char) cType() string        { return "char" }
func (t *char) goType() string       { return "byte" }
func (t *char) pasKind() string      { return "char" }
func (t *char) size() uintptr        { return unsafe.Sizeof(byte(0)) }

type integer struct {
	noder
}

func (t *integer) cardinality() uintptr { return math.MaxUint32 + 1 }
func (t *integer) cType() string        { return "int" }
func (t *integer) goType() string       { return "int32" }
func (t *integer) pasKind() string      { return "integer" }
func (t *integer) size() uintptr        { return unsafe.Sizeof(int32(0)) }

type real struct {
	noder
}

func (t *real) cType() string   { return "double" }
func (t *real) goType() string  { return "float64" }
func (t *real) pasKind() string { return "real" }
func (t *real) size() uintptr   { return unsafe.Sizeof(float64(0)) }

type boolean struct {
	noder
}

func (t *boolean) cardinality() uintptr { return 2 }
func (t *boolean) cType() string        { return "char" }
func (t *boolean) goType() string       { return "bool" }
func (t *boolean) pasKind() string      { return "boolean" }
func (t *boolean) size() uintptr        { return unsafe.Sizeof(byte(0)) }

type pasString struct {
	noder
	sz uintptr
}

func newPasStringFromSize(sz uintptr) *pasString {
	if x := stringTypes[sz]; x != nil {
		return x
	}

	t := &pasString{sz: sz}
	stringTypes[sz] = t
	return t
}

func (t *pasString) cType() string   { return fmt.Sprintf("char[%d]", t.sz) }
func (t *pasString) goType() string  { return fmt.Sprintf("[%d]byte", t.sz) }
func (t *pasString) pasKind() string { return "string" }
func (t *pasString) size() uintptr   { return t.sz }

type subrange struct {
	noder
	lo, hi int
	sz     uintptr
	cNm    string
	goNm   string
	card   uintptr
}

func newSubrange(lo, hi int) (*subrange, error) {
	r := &subrange{lo: lo, hi: hi}
	switch {
	case lo >= 0 && hi >= 0:
		switch {
		case hi <= math.MaxUint8:
			r.cNm = "char"
			r.goNm = "byte"
			r.sz = unsafe.Sizeof(byte(0))
			r.card = math.MaxUint8 + 1
		case hi <= math.MaxUint16:
			r.cNm = "short"
			r.goNm = "uint16"
			r.sz = unsafe.Sizeof(uint16(0))
			r.card = math.MaxUint16 + 1
		case hi <= math.MaxUint32:
			r.cNm = "unsigned"
			r.goNm = "uint32"
			r.sz = unsafe.Sizeof(uint32(0))
			r.card = math.MaxUint32 + 1
		default:
			return r, fmt.Errorf("subranges with cardinality exceeding 32 bits not supported")
		}
	default:
		return r, fmt.Errorf("signed subranges not supported")
	}
	return r, nil
}

func (t *subrange) cardinality() uintptr { return t.card }
func (t *subrange) cType() string        { return t.cNm }
func (t *subrange) goType() string       { return t.goNm }
func (t *subrange) pasKind() string      { return "subrange" }
func (t *subrange) size() uintptr        { return t.sz }

type file struct {
	noder
	component typ

	packed bool
}

func newFile(component typ) *file { return &file{component: component} }

func (t *file) cType() string   { return "void*[2]" }
func (t *file) goType() string  { return "io.ReadWriter" }
func (t *file) pasKind() string { return "file" }
func (t *file) setPacked()      { t.packed = true }
func (t *file) size() uintptr   { return unsafe.Sizeof(interface{}(nil)) }

type field struct {
	name string
	off  uintptr //TODO
	typ
}

type record struct {
	noder
	cachedCType  string
	cachedGoType string //TODO
	ccType       cc.Type
	sz           uintptr
	tagS         string

	packed     bool
	singleItem bool
}

func newRecord(goos, goarch string, fieldList *fieldList, tag string) (*record, error) {
	r := &record{tagS: tag}
	if tag == "" {
		return r, fmt.Errorf("anonymous record types not supported")
	}

	var b strings.Builder
	w := strutil.IndentFormatter(&b, "\t")
	fieldList.c(w, true, tag)
	r.cachedCType = strings.TrimSpace(strings.ReplaceAll(b.String(), "\n\n", "\n"))
	abi, err := cc.NewABI(goos, goarch)
	if err != nil {
		return r, err
	}

	cfg := &cc.Config{ABI: abi}
	ast, err := cc.Translate(cfg, nil, nil, []cc.Source{{Name: "record.c", Value: r.cachedCType + ";"}})
	if err != nil {
		return r, err
	}

	r.ccType = ast.StructTypes[cc.String(tag)]
	r.sz = r.ccType.Size()
	if fieldList.variantPart != nil {
		fld, ok := r.ccType.FieldByName(idU)
		if !ok {
			return r, fmt.Errorf("internal error: failed to find variant part type")
		}

		fieldList.variantPart.align = uintptr(fld.Type().Align())
		fieldList.variantPart.size = fld.Type().Size()
	}
	b.Reset()
	if err := fieldList.goStructLiteral(strutil.IndentFormatter(&b, "\t")); err != nil {
		return r, err
	}

	r.cachedGoType = strings.TrimSpace(strings.ReplaceAll(b.String(), "\n\n", "\n"))
	return r, nil
}

func (t *record) cType() string {
	if t.tagS != "" {
		return fmt.Sprintf("struct %s", t.tagS)
	}

	return t.cachedCType
}

func (t *record) goType() string {
	if t.tagS != "" {
		return t.tagS
	}

	return t.cachedGoType
}

func (t *record) pasKind() string { return "record" }
func (t *record) setPacked()      { t.packed = true }
func (t *record) size() uintptr   { return t.sz }
func (t *record) tag() string     { return t.tagS }

type array struct {
	noder
	dims []typ
	elem typ
	sz   uintptr

	packed bool
}

func newArray(n node, dims []*typeNode, elem typ) (*array, error) {
	r := &array{elem: elem, sz: elem.size()}
	for _, v := range dims {
		r.dims = append(r.dims, v.typ)
		ordinal, ok := v.typ.(ordinal)
		if !ok {
			return r, fmt.Errorf("ordinal type required: %s", v.typ.pasKind())
		}

		r.sz *= ordinal.cardinality()
	}
	return r, nil
}

func (t *array) cType() string   { panic(todo("")) }
func (t *array) goType() string  { panic(todo("")) }
func (t *array) pasKind() string { return "array" }
func (t *array) setPacked()      { t.packed = true }
func (t *array) size() uintptr   { return t.sz }

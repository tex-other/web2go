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
	_ typ = (*subrange)(nil)

	_ = []adder{aBoolean, aInteger, aReal, aSubrange}
	_ = []multiplier{aBoolean, aInteger, aReal, aSubrange}
	_ = []invertor{aInteger, aReal, aSubrange}
	_ = []negator{aBoolean}
	_ = []ordinal{aInteger, aSubrange, aChar}
	_ = []packer{aArray, aFile, aRecord}
	_ = []relator{aChar, aInteger, aReal, aSubrange}

	aArray    = &array{}
	aBoolean  = &boolean{}
	aChar     = &char{}
	aFile     = &file{}
	aInteger  = &integer{}
	aReal     = &real{}
	aRecord   = &record{}
	aSubrange = &subrange{}
	aString   = &array{ // pseudo type used by predeclared built-ins only.
		dims:     []typ{aInteger},
		elem:     aChar,
		sz:       0, // indicates open array, invalid otherwise
		isPacked: true,
		isString: true,
	}

	idRecord = cc.String("record")
	idU      = cc.String("__u__")
)

type typ interface {
	String() string
	cType() string
	canBeAssignedFrom(rhs typ) bool
	goType() string
	node
	render() string
	size() uintptr
}

type ordinal interface {
	cardinality() uint64
}

type packer interface {
	setPacked()
}

type tagger interface {
	tag() string
}

type invertor interface {
	inverse() (typ, error)
}

type relator interface {
	eq(relator) (typ, error)
	ne(relator) (typ, error)
	lt(relator) (typ, error)
	le(relator) (typ, error)
	gt(relator) (typ, error)
	ge(relator) (typ, error)
	in(relator) (typ, error)
}

type adder interface {
	add(adder) (typ, error)
	or(adder) (typ, error)
	sub(adder) (typ, error)
}

type multiplier interface {
	mul(multiplier) (typ, error)
	idiv(multiplier) (typ, error)
	div(multiplier) (typ, error)
	mod(multiplier) (typ, error)
	and(multiplier) (typ, error)
}

type negator interface {
	not() (typ, error)
}

type noder struct{}

func (noder) Position() (r token.Position) { return r }

func ndefOp(t typ) (typ, error) {
	return t, fmt.Errorf("operation not defined/supported on %s", t)
}

type char struct {
	noder
}

func (t *char) String() string              { return "char" }
func (t *char) cType() string               { return "char" }
func (t *char) cardinality() uint64         { return math.MaxUint8 + 1 }
func (t *char) eq(rhs relator) (typ, error) { return aBoolean, nil }
func (t *char) ge(rhs relator) (typ, error) { return aBoolean, nil }
func (t *char) goType() string              { return "byte" }
func (t *char) gt(rhs relator) (typ, error) { return aBoolean, nil }
func (t *char) in(rhs relator) (typ, error) { return ndefOp(t) }
func (t *char) le(rhs relator) (typ, error) { return aBoolean, nil }
func (t *char) lt(rhs relator) (typ, error) { return aBoolean, nil }
func (t *char) ne(rhs relator) (typ, error) { return aBoolean, nil }
func (t *char) render() string              { return t.goType() }
func (t *char) size() uintptr               { return unsafe.Sizeof(byte(0)) }

func (t *char) canBeAssignedFrom(rhs typ) bool {
	switch rhs.(type) {
	case *char:
		return true
	}

	return false
}

type integer struct {
	noder
}

func (t *integer) String() string                   { return "integer" }
func (t *integer) add(rhs adder) (typ, error)       { return t.binOp(rhs.(typ)) }
func (t *integer) and(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *integer) cType() string                    { return "int" }
func (t *integer) cardinality() uint64              { return math.MaxUint32 + 1 }
func (t *integer) div(rhs multiplier) (typ, error)  { return aReal, nil }
func (t *integer) eq(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *integer) ge(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *integer) goType() string                   { return "int32" }
func (t *integer) gt(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *integer) idiv(rhs multiplier) (typ, error) { return t.checkInt(rhs.(typ)) }
func (t *integer) in(rhs relator) (typ, error)      { return ndefOp(t) }
func (t *integer) inverse() (typ, error)            { return t, nil }
func (t *integer) le(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *integer) lt(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *integer) mod(rhs multiplier) (typ, error)  { return t.checkInt(rhs.(typ)) }
func (t *integer) mul(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *integer) ne(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *integer) or(rhs adder) (typ, error)        { return t.checkInt(rhs.(typ)) }
func (t *integer) render() string                   { return t.goType() }
func (t *integer) size() uintptr                    { return unsafe.Sizeof(int32(0)) }
func (t *integer) sub(rhs adder) (typ, error)       { return t.binOp(rhs.(typ)) }

func (t *integer) canBeAssignedFrom(rhs typ) bool {
	switch rhs.(type) {
	case *integer, *subrange:
		return true
	}

	return false
}

func (t *integer) checkInt(rhs typ) (typ, error) {
	switch rhs.(type) {
	case *integer, *subrange:
		return t, nil
	default:
		return t, fmt.Errorf("expected ordinal type: %s", rhs)
	}
}

func (t *integer) binOp(rhs typ) (typ, error) {
	switch rhs.(type) {
	case *integer, *subrange:
		return aInteger, nil
	case *real:
		return aReal, nil
	default:
		return ndefOp(rhs)
	}
}

type real struct {
	noder
}

func (t *real) canBeAssignedFrom(rhs typ) bool {
	switch rhs.(type) {
	case *real, *integer, *subrange:
		return true
	}

	return false
}

func (t *real) String() string                   { return "real" }
func (t *real) add(rhs adder) (typ, error)       { return t.binOp(rhs.(typ)) }
func (t *real) and(rhs multiplier) (typ, error)  { return ndefOp(t) }
func (t *real) cType() string                    { return "float" }
func (t *real) div(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *real) eq(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *real) ge(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *real) goType() string                   { return "float32" }
func (t *real) gt(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *real) idiv(rhs multiplier) (typ, error) { return ndefOp(t) }
func (t *real) in(rhs relator) (typ, error)      { return ndefOp(t) }
func (t *real) inverse() (typ, error)            { return t, nil }
func (t *real) le(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *real) lt(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *real) mod(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *real) mul(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *real) ne(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *real) or(rhs adder) (typ, error)        { return ndefOp(t) }
func (t *real) render() string                   { return t.goType() }
func (t *real) size() uintptr                    { return unsafe.Sizeof(float32(0)) }
func (t *real) sub(rhs adder) (typ, error)       { return t.binOp(rhs.(typ)) }

func (t *real) binOp(rhs typ) (typ, error) {
	switch rhs.(type) {
	case *real, *integer:
		return aReal, nil
	default:
		return ndefOp(t)
	}
}

type boolean struct {
	noder
}

func (t *boolean) canBeAssignedFrom(rhs typ) bool {
	switch rhs.(type) {
	case *boolean:
		return true
	}

	return false
}

func (t *boolean) String() string                   { return "boolean" }
func (t *boolean) add(rhs adder) (typ, error)       { return ndefOp(t) }
func (t *boolean) and(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *boolean) cType() string                    { return "char" }
func (t *boolean) cardinality() uint64              { return 2 }
func (t *boolean) div(rhs multiplier) (typ, error)  { return ndefOp(t) }
func (t *boolean) goType() string                   { return "bool" }
func (t *boolean) idiv(rhs multiplier) (typ, error) { return ndefOp(t) }
func (t *boolean) mod(rhs multiplier) (typ, error)  { return ndefOp(t) }
func (t *boolean) mul(rhs multiplier) (typ, error)  { return ndefOp(t) }
func (t *boolean) not() (typ, error)                { return t, nil }
func (t *boolean) or(rhs adder) (typ, error)        { return t.binOp(rhs.(typ)) }
func (t *boolean) render() string                   { return t.goType() }
func (t *boolean) size() uintptr                    { return unsafe.Sizeof(byte(0)) }
func (t *boolean) sub(rhs adder) (typ, error)       { return ndefOp(t) }

func (t *boolean) binOp(rhs typ) (typ, error) {
	switch rhs.(type) {
	case *boolean:
		return aBoolean, nil
	default:
		return ndefOp(rhs)
	}
}

type subrange struct {
	noder
	lo, hi int
	sz     uintptr
	cNm    string
	goNm   string
	card   uint64
}

func newSubrange(lo, hi int) (*subrange, error) {
	r := &subrange{lo: lo, hi: hi}
	if lo > hi {
		return r, fmt.Errorf("invalid subrange, lower bound above higher bound")
	}

	r.card = uint64(hi - lo + 1)
	switch {
	case lo >= 0 && hi >= 0:
		switch {
		case hi <= math.MaxUint8:
			r.cNm = "unsigned char"
			r.goNm = "byte"
			r.sz = unsafe.Sizeof(byte(0))
		case hi <= math.MaxUint16:
			r.cNm = "unsigned short"
			r.goNm = "uint16"
			r.sz = unsafe.Sizeof(uint16(0))
		case int64(hi) <= math.MaxUint32:
			r.cNm = "unsigned"
			r.goNm = "uint32"
			r.sz = unsafe.Sizeof(uint32(0))
		default:
			return r, fmt.Errorf("subranges with cardinality exceeding 32 bits not supported")
		}
	default:
		switch {
		case lo >= math.MinInt8 && hi <= math.MaxInt8:
			r.cNm = "signed char"
			r.goNm = "int8"
			r.sz = unsafe.Sizeof(int8(0))
		case lo >= math.MinInt16 && hi <= math.MaxInt16:
			r.cNm = "signed short"
			r.goNm = "int16"
			r.sz = unsafe.Sizeof(int16(0))
		case lo >= math.MinInt32 && hi <= math.MaxInt32:
			r.cNm = "int"
			r.goNm = "int32"
			r.sz = unsafe.Sizeof(int32(0))
		default:
			return r, fmt.Errorf("subranges with cardinality exceeding 32 bits not supported")
		}
	}
	return r, nil
}

func (t *subrange) String() string                   { return fmt.Sprintf("%d..%d", t.lo, t.hi) }
func (t *subrange) add(rhs adder) (typ, error)       { return t.binOp(rhs.(typ)) }
func (t *subrange) and(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *subrange) cType() string                    { return t.cNm }
func (t *subrange) cardinality() uint64              { return t.card }
func (t *subrange) div(rhs multiplier) (typ, error)  { return aReal, nil }
func (t *subrange) eq(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *subrange) ge(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *subrange) goType() string                   { return t.goNm }
func (t *subrange) gt(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *subrange) idiv(rhs multiplier) (typ, error) { return t.checkInt(rhs.(typ)) }
func (t *subrange) in(rhs relator) (typ, error)      { return ndefOp(t) }
func (t *subrange) inverse() (typ, error)            { return t, nil }
func (t *subrange) le(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *subrange) lt(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *subrange) mod(rhs multiplier) (typ, error)  { return t.checkInt(rhs.(typ)) }
func (t *subrange) mul(rhs multiplier) (typ, error)  { return t.binOp(rhs.(typ)) }
func (t *subrange) ne(rhs relator) (typ, error)      { return aBoolean, nil }
func (t *subrange) or(rhs adder) (typ, error)        { return t.checkInt(rhs.(typ)) }
func (t *subrange) render() string                   { return t.goType() }
func (t *subrange) size() uintptr                    { return t.sz }
func (t *subrange) sub(rhs adder) (typ, error)       { return t.binOp(rhs.(typ)) }

func (t *subrange) canBeAssignedFrom(rhs typ) bool {
	switch rhs.(type) {
	case *integer, *subrange:
		return true
	}

	return false
}

func (t *subrange) checkInt(rhs typ) (typ, error) {
	switch rhs.(type) {
	case *integer, *subrange:
		return aInteger, nil
	default:
		return ndefOp(rhs)
	}
}

func (t *subrange) binOp(rhs typ) (typ, error) {
	switch rhs.(type) {
	case *integer, *subrange:
		return aInteger, nil
	default:
		return ndefOp(rhs)
	}
}

type file struct {
	noder
	component typ

	packed bool
}

func newFile(component typ) *file { return &file{component: component} }

func (t *file) String() string { return fmt.Sprintf("file of %v", t.component) }
func (t *file) cType() string  { return "void*" }
func (t *file) goType() string { return "*pasFile" }
func (t *file) render() string { return t.goType() }
func (t *file) setPacked()     { t.packed = true }
func (t *file) size() uintptr  { return unsafe.Sizeof(interface{}(nil)) }

func (t *file) canBeAssignedFrom(rhs typ) bool {
	switch x := rhs.(type) {
	case *file:
		return t.component == nil || t.component.canBeAssignedFrom(x.component)
	}

	return false
}

type field struct {
	name string
	off  uintptr // wrt the variant field
	typ

	isVariant bool
}

type record struct {
	noder
	cachedCType  string
	cachedGoType string
	ccType       cc.Type
	fields       map[string]*field
	sz           uintptr
	tagS         string
	variants     []*field

	packed     bool
	singleItem bool
}

func newRecord(goos, goarch string, fieldList *fieldList, tag string) (*record, error) {
	r := &record{
		fields: map[string]*field{},
		tagS:   goIdent(tag),
	}
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
	if _, err := fieldList.collect(r.fields); err != nil {
		return r, err
	}

	if fieldList.variantPart != nil {
		unionField, ok := r.ccType.FieldByName(idU)
		if !ok {
			return r, fmt.Errorf("internal error: failed to find variant part type")
		}

		fieldList.variantPart.align = uintptr(unionField.Type().Align())
		fieldList.variantPart.size = unionField.Type().Size()
		if r.variants, err = fieldList.variantPart.collect(r.fields); err != nil {
			return r, err
		}

		u := unionField.Type()
		for _, v := range r.variants {
			f, ok := u.FieldByName(cc.String(strings.ToUpper(v.name)))
			if !ok {
				return r, fmt.Errorf("internal error: failed to find variant field: %s", v.name)
			}

			v.off = f.Offset()
			v.isVariant = true
		}
	}
	b.Reset()
	if err := fieldList.structLiteral(strutil.IndentFormatter(&b, "\t")); err != nil {
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

func (t *record) canBeAssignedFrom(rhs typ) bool {
	switch x := rhs.(type) {
	case *record:
		return t.goType() == x.goType()
	}

	return false
}

func (t *record) String() string { return fmt.Sprintf("record %s", t.tagS) }
func (t *record) render() string { return t.cachedGoType }
func (t *record) setPacked()     { t.packed = true }
func (t *record) size() uintptr  { return t.sz }
func (t *record) tag() string    { return t.tagS }

type array struct {
	noder
	dims []typ
	elem typ
	sz   uintptr

	isPacked bool
	isString bool
}

func newArray(n node, dims []*typeNode, elem typ) (*array, error) {
	r := &array{elem: elem, sz: elem.size()}
	for _, v := range dims {
		r.dims = append(r.dims, v.typ)
		ordinal, ok := v.typ.(ordinal)
		if !ok {
			return r, fmt.Errorf("ordinal type required: %s", v.typ.String())
		}

		r.sz *= uintptr(ordinal.cardinality())
	}
	if r.sz == 0 {
		return r, fmt.Errorf("internal error: array has zero size")
	}

	return r, nil
}

func (t *array) cType() string {
	var a []string
	for _, v := range t.dims {
		a = append(a, fmt.Sprintf("[%d]", v.(ordinal).cardinality()))
	}
	return fmt.Sprintf("%s%s", t.elem.cType(), strings.Join(a, ""))
}

func (t *array) goType() string {
	var a []string
	for _, v := range t.dims {
		a = append(a, fmt.Sprintf("[%d]", v.(ordinal).cardinality()))
	}
	return fmt.Sprintf("%s%s", strings.Join(a, ""), strings.TrimLeft(t.elem.goType(), "*"))
}

func (t *array) String() string {
	if t.isString {
		return fmt.Sprintf("array[%d] or char", t.sz)
	}

	return fmt.Sprintf("array%v of %v", t.dims, t.elem)
}

func (t *array) render() string { return t.goType() }
func (t *array) setPacked()     { t.isPacked = true }
func (t *array) size() uintptr  { return t.sz }

func (t *array) canBeAssignedFrom(rhs typ) bool {
	switch x := rhs.(type) {
	case *array:
		if (t.size() >= x.size() || t.isString && t.size() == 0) &&
			len(t.dims) == len(x.dims) &&
			t.elem.size() == x.elem.size() &&
			t.isPacked == x.isPacked {

			for i, v := range t.dims {
				if !v.canBeAssignedFrom(x.dims[i]) {
					return false
				}
			}

			return true
		}
	}

	return false
}

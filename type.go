// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"go/token"

	"modernc.org/mathutil"
)

var (
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
}

type noder struct{}

func (noder) Position() (r token.Position) { return r }

type char struct {
	noder
}

func (t *char) goName() string { return "byte" }
func (t *char) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }

type integer struct {
	noder
}

func (t *integer) goName() string { return "int32" }
func (t *integer) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }

type real struct {
	noder
}

func (t *real) goName() string { return "float64" }
func (t *real) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }

type boolean struct {
	noder
}

func (t *boolean) goName() string { return "bool" }
func (t *boolean) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }

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

func (t *stringType) goName() string { return fmt.Sprintf("[%d]byte", t.sz) }
func (t *stringType) setPacked()     { panic(todo("internal error: setPacked of a wrong type: %T", t)) }

type subrange struct {
	noder
	lo, hi int
}

func newSubrange(lo, hi int) *subrange {
	return &subrange{lo: lo, hi: hi}
}

func (t *subrange) goName() string {
	var lo, hi string
	if t.lo < 0 {
		lo = "M"
	}
	if t.hi < 0 {
		hi = "M"
	}
	return fmt.Sprintf("int%s%dTo%s%d", lo, abs(t.lo), hi, abs(t.hi))
}

func (t *subrange) setPacked() { panic(todo("internal error: setPacked of a wrong type: %T", t)) }

func abs(n int) int {
	if n >= 0 {
		return n
	}

	if n == mathutil.MinInt {
		panic(todo("internal error"))
	}

	return -n
}

type file struct {
	noder
	component typ

	packed bool
}

func newFile(component typ) *file { return &file{component: component} }

func (t *file) goName() string { return "io.ReadWriter" }
func (t *file) setPacked()     { t.packed = true }

type field struct {
	name string
	off  uintptr //TODO
	typ
}

type record struct {
	noder
	fields []field

	packed bool
}

func newRecord(fieldList *fieldList) *record {
	//TODO layout
	return &record{}
}

func (t *record) goName() string {
	panic(todo(""))
}

func (t *record) setPacked() { t.packed = true }

// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"

	"go/token"
)

var (
	_ typ = (*boolean)(nil)
	_ typ = (*char)(nil)
	_ typ = (*integer)(nil)
	_ typ = (*real)(nil)
	_ typ = (*stringType)(nil)

	aBoolean = &boolean{}
	aChar    = &char{}
	aInteger = &integer{}
	aReal    = &real{}

	stringTypes = map[int]*stringType{}
)

type typ interface {
	node
	goName() string
}

type noder struct{}

func (noder) Position() (r token.Position) { return r }

type char struct {
	noder
}

func (t *char) goName() string { return "byte" }

type integer struct {
	noder
}

func (t *integer) goName() string { return "int" }

type real struct {
	noder
}

func (t *real) goName() string { return "float64" }

type boolean struct {
	noder
}

func (t *boolean) goName() string { return "bool" }

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

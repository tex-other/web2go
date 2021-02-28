// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"strconv"

	"modernc.org/mathutil"
)

var (
	_ operand = booleanOperand(false)
	_ operand = integerOperand(0)
	_ operand = stringOperand("")
)

type operand interface {
	typ() typ
}

type booleanOperand bool

func (op booleanOperand) typ() typ { return aBoolean }

func newBooleanOperand(b bool) booleanOperand { return booleanOperand(b) }

type integerOperand int

func newIntegerOperandFromString(s string) (integerOperand, error) {
	n, err := strconv.ParseUint(s, 10, 64)
	if err != nil {
		return 0, err
	}

	if n > mathutil.MaxInt {
		return 0, fmt.Errorf("overflow: %s", s)
	}

	return integerOperand(n), nil
}

func (op integerOperand) typ() typ { return aInteger }

type stringOperand string

func newStringOperand(s string) stringOperand { return stringOperand(s) }

func (op stringOperand) typ() typ { return stringTypeForSize(len(op)) }

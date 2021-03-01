// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"math"
	"strconv"
)

var (
	_ operand = booleanOperand(false)
	_ operand = charOperand(0)
	_ operand = integerOperand(0)
	_ operand = realOperand(0)
	_ operand = stringOperand("")
)

type operand interface {
	typ() typ
}

type operandNegator interface {
	neg() operand
}

type booleanOperand bool

func (op booleanOperand) typ() typ { return aBoolean }

func newBooleanOperand(b bool) booleanOperand { return booleanOperand(b) }

type charOperand byte

func newCharOperand(b byte) charOperand { return charOperand(b) }

func (op charOperand) typ() typ { return aChar }

type integerOperand int32

func newIntegerOperandFromString(s string) (integerOperand, error) {
	n, err := strconv.ParseUint(s, 10, 32)
	if err != nil {
		return 0, err
	}

	if n > math.MaxInt32 {
		return 0, fmt.Errorf("overflow: %s", s)
	}

	return integerOperand(n), nil
}

func (op integerOperand) neg() operand { return -op }
func (op integerOperand) typ() typ     { return aInteger }

type stringOperand string

func newOperandFromString(s string) (operand, error) {
	switch len(s) {
	case 0:
		return stringOperand("invalid"), fmt.Errorf("empty strings not supported")
	case 1:
		return newCharOperand(s[0]), nil
	default:
		return stringOperand(s), nil
	}
}

func (op stringOperand) typ() typ { return newPasStringFromSize(uintptr(len(op))) }

type realOperand float64

func (op realOperand) typ() typ { return aReal }

func newRealOperandFromString(s string) (realOperand, error) {
	n, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return 0, err
	}

	return realOperand(n), nil
}

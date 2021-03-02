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
	_ literal = booleanLiteral(false)
	_ literal = charLiteral(0)
	_ literal = integerLiteral(0)
	_ literal = realLiteral(0)
	_ literal = stringLiteral("")

	stringLiteralTypes = map[int]*array{}
)

type literal interface {
	typ() typ
	render() string
}

type literalNegator interface {
	neg() literal
}

type booleanLiteral bool

func newBooleanLiteral(b bool) booleanLiteral { return booleanLiteral(b) }

func (l booleanLiteral) render() string { return fmt.Sprint(l) }
func (l booleanLiteral) typ() typ       { return aBoolean }

type charLiteral byte

func newCharLiterl(b byte) charLiteral { return charLiteral(b) }

func (l charLiteral) render() string { return strconv.QuoteRuneToASCII(rune(l)) }
func (l charLiteral) typ() typ       { return aChar }

type integerLiteral int32

func newIntegerLiteralFromString(s string) (integerLiteral, error) {
	n, err := strconv.ParseUint(s, 10, 32)
	if err != nil {
		return 0, err
	}

	if n > math.MaxInt32 {
		return 0, fmt.Errorf("overflow: %s", s)
	}

	return integerLiteral(n), nil
}

func (l integerLiteral) neg() literal   { return -l }
func (l integerLiteral) render() string { return fmt.Sprint(l) }
func (l integerLiteral) typ() typ       { return aInteger }

type stringLiteral string

// s is already un-escaped
func newLiteralFromString(s string) (literal, error) {
	switch len(s) {
	case 0:
		return stringLiteral("invalid"), fmt.Errorf("empty strings not supported")
	case 1:
		return newCharLiterl(s[0]), nil
	default:
		return stringLiteral(s), nil
	}
}

func (l stringLiteral) render() string { return strconv.QuoteToASCII(string(l)) }

func (l stringLiteral) typ() typ {
	if t := stringLiteralTypes[len(l)]; t != nil {
		return t
	}

	t := &array{
		dims:     []typ{aInteger},
		elem:     aChar,
		sz:       uintptr(len(l)),
		isPacked: true,
		isString: true,
	}
	stringLiteralTypes[len(l)] = t
	return t
}

type realLiteral float64

func newRealLiteralFromString(s string) (realLiteral, error) {
	n, err := strconv.ParseFloat(s, 64)
	if err != nil {
		return 0, err
	}

	return realLiteral(n), nil
}

func (l realLiteral) render() string { return fmt.Sprint(l) }
func (l realLiteral) typ() typ       { return aReal }

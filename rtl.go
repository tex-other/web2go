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
	"math"
)

type memoryWord struct {
	variant float64
}

/* CUT HERE */

var _ error = pasError(0)

const (
	pasEndOfTeX pasError = -iota - 1
	pasFinalEnd
)

type pasError int

func (pasError) Error() string { return "" }

func pasJumpOut() { panic(pasEndOfTeX) }

func abs(n float64) float64 { return math.Abs(n) } //TODO check the Pascal definition.

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

func round(n float64) int32 { return int32(math.Round(n)) } //TODO check the Pascal definition.

type vaWidth int

func read(args ...interface{}) {
	panic("TODO")
}

func readLn(args ...interface{}) {
	panic("TODO")
}

func write(args ...interface{}) {
	panic("TODO")
}

func writeLn(args ...interface{}) {
	panic("TODO")
}

type pasFile struct{} //TODO

func (f *pasFile) byte() byte {
	panic("TODO")
}

func (f *pasFile) memoryWord() memoryWord {
	panic("TODO")
}

func (f *pasFile) pMemoryWord() *memoryWord {
	panic("TODO")
}

func break1(f *pasFile) {
	panic("TODO")
}

func breakIn(f *pasFile, b bool) {
	panic("TODO")
}

func close(f *pasFile) {
	panic("TODO")
}

func eof(f *pasFile) bool {
	panic("TODO")
}

func eoln(f *pasFile) bool {
	panic("TODO")
}

func erstat(f *pasFile) int32 {
	panic("TODO")
}

func get(f *pasFile) {
	panic("TODO")
}

func put(f *pasFile) {
	panic("TODO")
}

func reset(f *pasFile, s1, s2 string) {
	panic(fmt.Errorf("reset(%q, %q) TODO", s1, s2))
}

func rewrite(f *pasFile, s1, s2 string) {
	panic(fmt.Errorf("rewrite(%q, %q) TODO", s1, s2))
}

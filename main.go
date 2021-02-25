// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// Command web2go is an attempt to mechanically translate tex.web to Go. (Work
// in progress.)
//
// This program is being developed and tested using a tex.web file annotated as
//
//	% Version 3.141592653 was similar but more extensive (January 2021)
//
// and will probably crash on any other version or modification of it.  It's a
// non-goal to handle anything else than this one file with this single
// version.
//
// Installation
//
// To install:
//
//     $ go install modernc.org/web2go
//
// Dependencies
//
// Some external programs might be used, depending on what extension the
// input-file has.
//
//	TANGLE(1)
//
//	NAME
//	       tangle - translate WEB to Pascal
//
//	SYNOPSIS
//	       tangle [options] webfile[.web] [changefile[.ch]]
//
// Used to convert .web to .p.
//
//	ptop(1)
//
//	NAME
//	       ptop - The FPC Pascal configurable source beautifier.
//
//	       Origin probably Pascal-TO-Pascal.
//
//	SYNOPSIS
//	       ptop[-v][-iindent][-bbufsize][-coptsfile]<infile><outfile>
//
// Used to optionally convert .p to .pas.
//
// Invocation
//
// To run the command:
//
//	$ web2go [options] input-file
//
// Input files
//
// A .web file will be processed by tangle to produce a .p file that will be
// the input of the transpiler. If the -ptop option is given, the .p file will
// by formatted by ptop to produce a .pas file which will be the transpiler
// input.
//
// A .p file will be the input file of the transpiler. If the -ptop option is
// given, the .p file will by formatted by ptop to produce a .pas file which
// will be the transpiler input.
//
// A .pas file is the input of the transpiler.
//
// Options
//
// Flags that adjust program begavior
//
// 	-o output-file
//
// Explicitly set the Go output file name. Existing files will be overwritten
// without asking.
//
//	-p file-name
//
// When processing a .web file, keep the resulting .p file in file-name.
// Existing files will be overwritten without asking.
//
//	-pas file-name
//
// When formatting a .p file, keep the resulting .pas file in file-name.
// Existing files will be overwritten without asking.
//
//	-ptop
//
// If the input-file is .web or .p, format the .p file using ptop to produce a
// .pas file that will be the input of the transpiler.  Requires ptop to be
// installed.
package main // import "modernc.org/web2go"

func main() {}

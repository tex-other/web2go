// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

// This is an example of how a package API in -lib mode may look.
//
//	user@box:~/src/modernc.org/web2go$ web2go -o lib/tex.go -lib tex.web    # add the engine
//	user@box:~/src/modernc.org/web2go$ go build lib/*.go                    # check the two-file package builds
//	user@box:~/src/modernc.org/web2go$

package tex

import (
	"io"
)

func Process(stdin io.ReadCloser, stdout io.WriteCloser) error {
	var tex tex
	tex.stdin = stdin
	tex.stdout = stdout
	tex.main()
	return nil
}

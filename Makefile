# Copyright 2021 The web2go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

.PHONY:	all clean edit editor later nuke todo

grep=--include=*.go
ngrep='TODOOK\|testdata\|TODO-'


all:
	@LC_ALL=C date
	@go version 2>&1 | tee log
	@gofmt -l -s -w *.go
	@go install -v ./...
	@go test -i
	@go test 2>&1 -timeout 1h | tee -a log
	@go vet 2>&1 | grep -v $(ngrep) || true
	@golint 2>&1 | grep -v $(ngrep) || true
	@make todo
	@misspell *.go
	@staticcheck | grep -v 'scanner\.go' || true
	@maligned || true
	@grep -n --color=always 'FAIL\|PASS' log 
	@LC_ALL=C date 2>&1 | tee -a log

clean:
	go clean
	rm -f *~ *.test *.out

edit:
	@touch log
	@if [ -f "Session.vim" ]; then gvim -S & else gvim -p Makefile *.go & fi

editor:
	gofmt -l -s -w *.go
	GO111MODULE=off nilness . ./... 2>&1 | tee log-install
	GO111MODULE=off go install -v ./... 2>&1 | tee -a log-install
	@gofmt -l -s -w .

later:
	@grep -n $(grep) LATER * || true
	@grep -n $(grep) MAYBE * || true

nuke: clean
	go clean -i

todo:
	@grep -nr $(grep) ^[[:space:]]*_[[:space:]]*=[[:space:]][[:alpha:]][[:alnum:]]* * | grep -v $(ngrep) || true
	@grep -nrw $(grep) 'TODO\|panic' * | grep -v $(ngrep) || true
	@grep -nr $(grep) BUG * | grep -v $(ngrep) || true
	@grep -nr $(grep) [^[:alpha:]]println * | grep -v $(ngrep) || true
	@grep -nir $(grep) 'work.*progress' || true

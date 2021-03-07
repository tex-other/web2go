# Copyright 2021 The web2go Authors. All rights reserved.
# Use of this source code is governed by a BSD-style
# license that can be found in the LICENSE file.

.PHONY:	all clean edit editor later nuke todo

grep=--include=*.go
ngrep='TODOOK\|testdata\|TODO-'


all:
	@LC_ALL=C date
	@go version 2>&1 | tee log
	@gofmt -l -s -w *.go 2>&1 | tee -a log
	@go install -v ./... 2>&1 | tee -a log
	@go test -i 2>&1 | tee -a log
	@go test 2>&1 -timeout 1h 2>&1 | tee -a log
	@go vet 2>&1 | grep -v $(ngrep) || true 2>&1 | tee -a log
	@golint 2>&1 | grep -v $(ngrep) || true 2>&1 | tee -a log
	@make todo 2>&1 | tee -a log
	@misspell *.go 2>&1 | tee -a log
	@nilness . ./... 2>&1 | tee -a log
	@staticcheck | grep -v 'scanner\.go' || true 2>&1 | tee -a log
	@maligned || true 2>&1 | tee -a log
	@LC_ALL=C date 2>&1 | tee -a log
	@grep -n --color=always 'FAIL\|PASS' log 

build_all_targets:
	GOOS=darwin GOARCH=amd64 go build -v -o /dev/null ./...
	GOOS=linux GOARCH=386 go build -v -o /dev/null ./...
	GOOS=linux GOARCH=amd64 go build -v -o /dev/null ./...
	GOOS=linux GOARCH=arm go build -v -o /dev/null ./...
	GOOS=linux GOARCH=arm64 go build -v -o /dev/null ./...
	GOOS=windows GOARCH=386 go build -v -o /dev/null ./...
	GOOS=windows GOARCH=amd64 go build -v -o /dev/null ./...
	echo done

clean:
	go clean
	rm -f *~ *.test *.out tex.p tex.pool tex.tex tex.log

edit:
	@touch log
	@if [ -f "Session.vim" ]; then gvim -S & else gvim -p Makefile *.go & fi

editor: stringer.go assets.go
	gofmt -l -s -w *.go 2>&1 | tee log
	GO111MODULE=off go test 2>&1 | tee -a log
	GO111MODULE=off go install -v 2>&1 | tee -a log
	@gofmt -l -s -w .

later:
	@grep -n $(grep) LATER * || true
	@grep -n $(grep) MAYBE * || true

nuke: clean
	go clean -i

xtex:
	tangle tex.web assets/changefile.ch
	cp tex.p ~/tmp/tex.p
	weave tex.web
	ptop tex.p tex.pas
	go install -v
	rm -rf ~/tmp/xtex.go
	web2go -o ~/tmp/xtex.go tex.web assets/changefile.ch
	go build -v -o ~/bin/xtex ~/tmp/xtex.go

todo:
	@grep -nr $(grep) ^[[:space:]]*_[[:space:]]*=[[:space:]][[:alpha:]][[:alnum:]]* * | grep -v $(ngrep) || true
	@grep -nrw $(grep) 'TODO\|panic' * | grep -v $(ngrep) || true
	@grep -nr $(grep) BUG * | grep -v $(ngrep) || true
	@grep -nr $(grep) [^[:alpha:]]println * | grep -v $(ngrep) || true
	@grep -nir $(grep) 'work.*progress' || true

stringer.go: scanner.go
	stringer -output stringer.go -linecomment -type=ch
	gofmt -l -s -w .

assets.go: assets/changefile.ch rtl.go
	cp -v rtl.go assets/
	assets
	gofmt -l -s -w .

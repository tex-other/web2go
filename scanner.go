// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"go/token"
	"strings"
)

type ch rune

func (l ch) str() string {
	if l < AND || l > WITH {
		return fmt.Sprintf("%#U", l)
	}

	return l.String()
}

const (
	AND          ch = iota + 0xe000 // and
	ARRAY                           // array
	ASSIGN                          // assign
	BEGIN                           // begin
	CASE                            // case
	CONST                           // const
	DD                              // ..
	DIV                             // div
	DO                              // do
	DOWNTO                          // downto
	ELSE                            // else
	END                             // end
	FILE                            // file
	FOR                             // for
	FUNCTION                        // function
	GE                              // >=
	GOTO                            // goto
	IDENTIFIER                      // identifier
	IF                              // if
	IN                              // in
	INT_LITERAL                     // integer literal
	LABEL                           // label
	LE                              // <=
	MOD                             // mod
	NE                              // <>
	NIL                             // nil
	NOT                             // not
	OF                              // of
	OR                              // or
	OTHERS                          // others
	PACKED                          // packed
	PROCEDURE                       // procedure
	PROGRAM                         // program
	REAL_LITERAL                    // real literal
	RECORD                          // record
	REPEAT                          // repeat
	SEP                             // separator
	SET                             // set
	STR_LITERAL                     // string literal
	THEN                            // then
	TO                              // to
	TYPE                            // type
	UNTIL                           // until
	VAR                             // var
	WHILE                           // while
	WITH                            // with
)

var keywords = map[string]ch{
	"and":       AND,
	"array":     ARRAY,
	"begin":     BEGIN,
	"case":      CASE,
	"const":     CONST,
	"div":       DIV,
	"do":        DO,
	"downto":    DOWNTO,
	"else":      ELSE,
	"end":       END,
	"file":      FILE,
	"for":       FOR,
	"function":  FUNCTION,
	"goto":      GOTO,
	"if":        IF,
	"in":        IN,
	"label":     LABEL,
	"mod":       MOD,
	"nil":       NIL,
	"not":       NOT,
	"of":        OF,
	"or":        OR,
	"others":    OTHERS,
	"packed":    PACKED,
	"procedure": PROCEDURE,
	"program":   PROGRAM,
	"record":    RECORD,
	"repeat":    REPEAT,
	"set":       SET,
	"then":      THEN,
	"to":        TO,
	"type":      TYPE,
	"until":     UNTIL,
	"var":       VAR,
	"while":     WHILE,
	"with":      WITH,
}

type node interface {
	Position() token.Position
}

type tok struct {
	ch
	file *token.File
	pos  token.Pos
	sep  string
	src  string
}

func (t *tok) Position() token.Position {
	return t.file.Position(t.pos)
}

func (t *tok) String() string {
	return fmt.Sprintf("%v: %q %s", t.Position(), t.src, t.ch.str())
}

type scanner struct {
	errs []string
	file *token.File
	last *tok
	pos  token.Pos
	s    string
	sep  string
	si   int // current index into b

	allErrors bool
}

func newScanner(b []byte, name string) (*scanner, error) {
	for i, v := range b {
		if v == 0 {
			return nil, fmt.Errorf("input file contains a zero byte at offset %#x", i)
		}

		if v > 127 {
			return nil, fmt.Errorf("input file contains a non-ASCII byte at offset %#x", i)
		}
	}

	fs := token.NewFileSet()
	file := fs.AddFile(name, -1, len(b))
	b = append(b, 0) // Set the sentinel.
	return &scanner{
		s:    string(b),
		file: file,
		pos:  file.Pos(0),
	}, nil
}

func (s *scanner) errList() error {
	if len(s.errs) == 0 {
		return nil
	}

	return fmt.Errorf("%s", strings.Join(s.errs, "\n"))
}

func (s *scanner) c() ch { return ch(s.s[s.si]) }

func (s *scanner) post() ch {
	r := s.s[s.si]
	if r != 0 {
		s.si++
	}
	return ch(r)
}

func (s *scanner) pre() ch {
	if s.s[s.si] != 0 {
		s.si++
	}
	return ch(s.s[s.si])
}

func (s *scanner) position() token.Position {
	return s.file.Position(s.pos)
}

func (s *scanner) err(msg string, args ...interface{}) {
	if !s.allErrors && len(s.errs) == 10 {
		return
	}

	p := fmt.Sprintf("%v: ", s.position())
	msg = fmt.Sprintf(p+msg, args...)
	trc("scanner err: %v", msg)
	s.errs = append(s.errs, msg)
}

// [0] 0.1
func (s *scanner) scan() (r *tok) {
	si0 := s.si
	defer func() {
		if r == nil {
			return
		}

		r.file = s.file
		r.pos = s.pos
		r.sep = s.sep
		r.src = s.s[si0:s.si]
		s.last = r
		s.sep = ""
		if r.ch == IDENTIFIER {
			if x, ok := keywords[strings.ToLower(r.src)]; ok {
				r.ch = x
			}
		}
	}()
more:
	si0 = s.si
	s.pos = s.file.Pos(si0)
	c := s.c()
	switch {
	case s.isSep(c):
		for s.isSep(s.pre()) {
		}
		s.sep = s.s[si0:s.si]
		goto more
	case isIDFirst(c):
		for isIDNext(s.pre()) {
		}
		return &tok{ch: IDENTIFIER}
	case isDigit(c):
		for {
			switch c := s.pre(); {
			case isDigit(c):
				// ok
			case c == '.':
				if s.pre() == '.' {
					s.si--
					return &tok{ch: INT_LITERAL}
				}

				for {
					c = s.pre()
					if isDigit(c) {
						continue
					}

					switch c {
					case 'e', 'E':
						panic(todo("%#+U", c))
					default:
						return &tok{ch: REAL_LITERAL}
					}

				}
			default:
				return &tok{ch: INT_LITERAL}
			}
		}
	}

	switch c {
	case ';', ',', '=', '(', ')', '+', '-', '*', '/', '[', ']', '^':
		s.post()
		return &tok{ch: c}
	case ':':
		if s.pre() == '=' {
			s.post()
			return &tok{ch: ASSIGN}
		}

		return &tok{ch: c}
	case '\'':
		for {
			switch s.pre() {
			case '\'':
				if s.pre() == '\'' {
					break
				}

				return &tok{ch: STR_LITERAL}
			case 0:
				s.err("unterminated comment")
				return &tok{ch: -1}
			}
		}
	case '.':
		if s.pre() == '.' {
			s.post()
			return &tok{ch: DD}
		}

		return &tok{ch: c}
	case '<':
		switch s.pre() {
		case '=':
			s.post()
			return &tok{ch: LE}
		case '>':
			s.post()
			return &tok{ch: NE}
		}

		return &tok{ch: c}
	case '>':
		if s.pre() == '=' {
			s.post()
			return &tok{ch: GE}
		}

		return &tok{ch: c}
	case '{':
		for {
			switch s.pre() {
			case '\n':
				s.file.AddLine(s.si + 1)
			case '}':
				s.post()
				return &tok{ch: SEP}
			case 0:
				s.err("unterminated comment")
				return &tok{ch: -1}
			}
		}
	case 0:
		s.pos--
		return &tok{ch: -1}
	default:
		panic(todo("%v: %#+U", s.position(), s.c()))
	}
}

func isDigit(c ch) bool   { return c >= '0' && c <= '9' }
func isIDFirst(c ch) bool { return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' }
func isIDNext(c ch) bool  { return isIDFirst(c) || c >= '0' && c <= '9' }

func (s *scanner) isSep(c ch) bool {
	switch c {
	case '\n':
		s.file.AddLine(s.si + 1)
		fallthrough
	case ' ':
		return true
	}

	return false
}

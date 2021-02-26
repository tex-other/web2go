// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"go/token"
	"strings"
)

type char rune

func (l char) str() string {
	if l < AND || l > WITH {
		return fmt.Sprintf("%#U", l)
	}

	return l.String()
}

const (
	AND          char = iota + 0xe000 // and
	ARRAY                             // array
	ASSIGN                            // assign
	BEGIN                             // begin
	CASE                              // case
	CONST                             // const
	DD                                // ..
	DIV                               // div
	DO                                // do
	DOWNTO                            // downto
	ELSE                              // else
	END                               // end
	FILE                              // file
	FOR                               // for
	FUNCTION                          // function
	GE                                // >=
	GOTO                              // goto
	IDENTIFIER                        // identifier
	IF                                // if
	IN                                // in
	INT_LITERAL                       // integer literal
	LABEL                             // label
	LE                                // <=
	MOD                               // mod
	NIL                               // nil
	NOT                               // not
	OF                                // of
	OR                                // or
	PACKED                            // packed
	PROCEDURE                         // procedure
	PROGRAM                           // program
	REAL_LITERAL                      // real literal
	RECORD                            // record
	REPEAT                            // repeat
	SEP                               // separator
	SET                               // set
	STR_LITERAL                       // string literal
	THEN                              // then
	TO                                // to
	TYPE                              // type
	UNTIL                             // until
	VAR                               // var
	WHILE                             // while
	WITH                              // with
)

var keywords = map[string]char{
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
	char char
	file *token.File
	pos  token.Pos
	sep  string
	src  string
}

func (t *tok) Position() token.Position {
	return t.file.Position(t.pos)
}

func (t *tok) String() string {
	return fmt.Sprintf("%v: %q %q %s", t.Position(), t.sep, t.src, t.char.str())
}

type scanner struct {
	errs []string
	file *token.File
	last *tok
	pos  token.Pos
	s    string
	sep  string
	si   int // current index into b
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

func (s *scanner) c() char { return char(s.s[s.si]) }

func (s *scanner) post() char {
	r := s.s[s.si]
	if r != 0 {
		s.si++
	}
	return char(r)
}

func (s *scanner) pre() char {
	if s.s[s.si] != 0 {
		s.si++
	}
	return char(s.s[s.si])
}

func (s *scanner) position() token.Position {
	return s.file.Position(s.pos)
}

func (s *scanner) err(msg string, args ...interface{}) {
	p := fmt.Sprintf("%v: ", s.position())
	s.errs = append(s.errs, fmt.Sprintf(p+msg, args...))
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
		if r.char == IDENTIFIER {
			if x, ok := keywords[strings.ToLower(r.src)]; ok {
				r.char = x
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
		return &tok{char: IDENTIFIER}
	case isDigit(c):
		for {
			switch c := s.pre(); {
			case isDigit(c):
				// ok
			case c == '.':
				if s.pre() == '.' {
					s.si--
					return &tok{char: INT_LITERAL}
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
						return &tok{char: REAL_LITERAL}
					}

				}
			default:
				return &tok{char: INT_LITERAL}
			}
		}
	}

	switch c {
	case ';', ',', '=', '(', ')', '+', '-', '*', '/', '[', ']', '^':
		s.post()
		return &tok{char: c}
	case ':':
		if s.pre() == '=' {
			s.post()
			return &tok{char: ASSIGN}
		}

		return &tok{char: c}
	case '\'':
		for {
			switch s.pre() {
			case '\'':
				s.post()
				return &tok{char: STR_LITERAL}
			case 0:
				s.err("unterminated comment")
				return &tok{char: -1}
			}
		}
	case '.':
		if s.pre() == '.' {
			s.post()
			return &tok{char: DD}
		}

		return &tok{char: c}
	case '<':
		if s.pre() == '=' {
			s.post()
			return &tok{char: LE}
		}

		return &tok{char: c}
	case '>':
		if s.pre() == '=' {
			s.post()
			return &tok{char: GE}
		}

		return &tok{char: c}
	case '{':
		for {
			switch s.pre() {
			case '\n':
				s.file.AddLine(s.si + 1)
			case '}':
				s.post()
				return &tok{char: SEP}
			case 0:
				s.err("unterminated comment")
				return &tok{char: -1}
			}
		}
	case 0:
		s.pos--
		return &tok{char: -1}
	default:
		panic(todo("%v: %#+U", s.position(), s.c()))
	}
}

func isDigit(c char) bool   { return c >= '0' && c <= '9' }
func isIDFirst(c char) bool { return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' }
func isIDNext(c char) bool  { return isIDFirst(c) || c >= '0' && c <= '9' }

func (s *scanner) isSep(c char) bool {
	switch c {
	case '\n':
		s.file.AddLine(s.si + 1)
		fallthrough
	case ' ':
		return true
	}

	return false
}

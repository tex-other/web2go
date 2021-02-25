// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"bytes"
	"fmt"
	"go/token"
	"strings"
)

var keywords = map[string]rune{
	"and":       AND,
	"array":     ARRAY,
	"begin":     BEGIN,
	"boolean":   BOOLEAN,
	"case":      CASE,
	"const":     CONST,
	"div":       DIV,
	"do":        DO,
	"else":      ELSE,
	"end":       END,
	"false":     FALSE,
	"file":      FILE,
	"for":       FOR,
	"forward":   FORWARD,
	"function":  FUNCTION,
	"goto":      GOTO,
	"if":        IF,
	"integer":   INTEGER,
	"label":     LABEL,
	"not":       NOT,
	"of":        OF,
	"or":        OR,
	"packed":    PACKED,
	"procedure": PROCEDURE,
	"program":   PROGRAM,
	"real":      REAL,
	"record":    RECORD,
	"repeat":    REPEAT,
	"then":      THEN,
	"to":        TO,
	"true":      TRUE,
	"var":       VAR,
	"while":     WHILE,
}

type node interface {
	Position() token.Position
}

type tok struct {
	file *token.File
	pos  token.Pos
	rune rune
	sep  string
	src  string
}

func (t *tok) Position() token.Position {
	return t.file.Position(t.pos)
}

func (t *tok) str() string {
	return fmt.Sprintf("%v: %q %q %s", t.Position(), t.sep, t.src, yySymName(int(t.rune)))
}

func prettyString(n node) string {
	panic(todo(""))
}

type scanner struct {
	errs []error
	file *token.File
	pos  token.Pos
	s    string
	sep  string
	si   int // current index into b
}

func newScanner(b []byte, name string) (*scanner, error) {
	if x := bytes.IndexByte(b, 0); x >= 0 {
		return nil, fmt.Errorf("input file contains a zero byte at offset %#x", x)
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

func (s *scanner) c() byte { return s.s[s.si] }

func (s *scanner) post() byte {
	r := s.s[s.si]
	if r != 0 {
		s.si++
	}
	return r
}

func (s *scanner) pre() byte {
	if s.s[s.si] != 0 {
		s.si++
	}
	return s.s[s.si]
}

func (s *scanner) position() token.Position {
	return s.file.Position(s.pos)
}

func (s *scanner) err(msg string, args ...interface{}) {
	p := fmt.Sprintf("%v: ", s.position())
	s.errs = append(s.errs, fmt.Errorf(p+msg, args...))
}

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
		s.sep = ""
		if r.rune == IDENTIFIER {
			if x, ok := keywords[strings.ToLower(r.src)]; ok {
				r.rune = x
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
	case isIdFirst(c):
		for isIdNext(s.pre()) {
		}
		return &tok{rune: IDENTIFIER}
	case isDigit(c):
		for {
			switch c := s.pre(); {
			case isDigit(c):
				// ok
			case c == '.':
				if s.pre() == '.' {
					s.si--
					return &tok{rune: INT_LITERAL}
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
						return &tok{rune: REAL_LITERAL}
					}

				}
			default:
				return &tok{rune: INT_LITERAL}
			}
		}
	}

	switch c {
	case ';', ',', '=', '(', ')', '+', '-', '*', '/', '[', ']', '^':
		s.post()
		return &tok{rune: rune(c)}
	case ':':
		if s.pre() == '=' {
			s.post()
			return &tok{rune: ASSIGN}
		}

		return &tok{rune: rune(c)}
	case '\'':
		for {
			switch s.pre() {
			case '\'':
				s.post()
				return &tok{rune: STR_LITERAL}
			case 0:
				s.err("unterminated comment")
				return &tok{rune: -1}
			}
		}
	case '.':
		if s.pre() == '.' {
			s.post()
			return &tok{rune: DD}
		}

		return &tok{rune: rune(c)}
	case '<':
		if s.pre() == '=' {
			s.post()
			return &tok{rune: LE}
		}

		return &tok{rune: rune(c)}
	case '>':
		if s.pre() == '=' {
			s.post()
			return &tok{rune: GE}
		}

		return &tok{rune: rune(c)}
	case '{':
		for {
			switch s.pre() {
			case '\n':
				s.file.AddLine(s.si + 1)
			case '}':
				s.post()
				return &tok{rune: SEP}
			case 0:
				s.err("unterminated comment")
				return &tok{rune: -1}
			}
		}
	case 0:
		s.pos--
		return &tok{rune: -1}
	default:
		panic(todo("%v: %#+U", s.position(), s.c()))
	}
}

func isDigit(c byte) bool   { return c >= '0' && c <= '9' }
func isIdFirst(c byte) bool { return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' }
func isIdNext(c byte) bool  { return isIdFirst(c) || c >= '0' && c <= '9' }

func (s *scanner) isSep(c byte) bool {
	switch c {
	case '\n':
		s.file.AddLine(s.si + 1)
		fallthrough
	case ' ':
		return true
	}

	return false
}

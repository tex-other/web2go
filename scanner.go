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

const (
	_ = iota
	AND
	ARRAY
	BEGIN
	CASE
	CONST
	DIV
	DO
	DOWNTO
	ELSE
	END
	FILE
	FOR
	FUNCTION
	GOTO
	IF
	IN
	LABEL
	MOD
	NIL
	NOT
	OF
	OR
	PACKED
	PROCEDURE
	PROGRAM
	RECORD
	REPEAT
	SET
	THEN
	TO
	TYPE
	UNTIL
	VAR
	WHILE
	WITH
	IDENTIFIER
	INT_LITERAL
	ASSIGN
	REAL_LITERAL
	STR_LITERAL
	DD
	LE
	GE
	SEP
)

var keywords = map[string]rune{

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
	file *token.File
	pos  token.Pos
	rune rune
	sep  string
	src  string
}

func (t *tok) Position() token.Position {
	return t.file.Position(t.pos)
}

type lexer struct {
	errs []string
	file *token.File
	last *tok
	pos  token.Pos
	s    string
	sep  string
	si   int // current index into b
}

func newLexer(b []byte, name string) (*lexer, error) {
	if x := bytes.IndexByte(b, 0); x >= 0 {
		return nil, fmt.Errorf("input file contains a zero byte at offset %#x", x)
	}

	fs := token.NewFileSet()
	file := fs.AddFile(name, -1, len(b))
	b = append(b, 0) // Set the sentinel.
	return &lexer{
		s:    string(b),
		file: file,
		pos:  file.Pos(0),
	}, nil
}

func (l *lexer) c() byte { return l.s[l.si] }

func (l *lexer) post() byte {
	r := l.s[l.si]
	if r != 0 {
		l.si++
	}
	return r
}

func (l *lexer) pre() byte {
	if l.s[l.si] != 0 {
		l.si++
	}
	return l.s[l.si]
}

func (l *lexer) position() token.Position {
	return l.file.Position(l.pos)
}

func (l *lexer) err(msg string, args ...interface{}) {
	p := fmt.Sprintf("%v: ", l.position())
	l.errs = append(l.errs, fmt.Sprintf(p+msg, args...))
}

// [0] 0.1
func (l *lexer) scan() (r *tok) {
	si0 := l.si
	defer func() {
		if r == nil {
			return
		}

		r.file = l.file
		r.pos = l.pos
		r.sep = l.sep
		r.src = l.s[si0:l.si]
		l.last = r
		l.sep = ""
		if r.rune == IDENTIFIER {
			if x, ok := keywords[strings.ToLower(r.src)]; ok {
				r.rune = x
			}
		}
	}()
more:
	si0 = l.si
	l.pos = l.file.Pos(si0)
	c := l.c()
	switch {
	case l.isSep(c):
		for l.isSep(l.pre()) {
		}
		l.sep = l.s[si0:l.si]
		goto more
	case isIdFirst(c):
		for isIdNext(l.pre()) {
		}
		return &tok{rune: IDENTIFIER}
	case isDigit(c):
		for {
			switch c := l.pre(); {
			case isDigit(c):
				// ok
			case c == '.':
				if l.pre() == '.' {
					l.si--
					return &tok{rune: INT_LITERAL}
				}

				for {
					c = l.pre()
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
		l.post()
		return &tok{rune: rune(c)}
	case ':':
		if l.pre() == '=' {
			l.post()
			return &tok{rune: ASSIGN}
		}

		return &tok{rune: rune(c)}
	case '\'':
		for {
			switch l.pre() {
			case '\'':
				l.post()
				return &tok{rune: STR_LITERAL}
			case 0:
				l.err("unterminated comment")
				return &tok{rune: -1}
			}
		}
	case '.':
		if l.pre() == '.' {
			l.post()
			return &tok{rune: DD}
		}

		return &tok{rune: rune(c)}
	case '<':
		if l.pre() == '=' {
			l.post()
			return &tok{rune: LE}
		}

		return &tok{rune: rune(c)}
	case '>':
		if l.pre() == '=' {
			l.post()
			return &tok{rune: GE}
		}

		return &tok{rune: rune(c)}
	case '{':
		for {
			switch l.pre() {
			case '\n':
				l.file.AddLine(l.si + 1)
			case '}':
				l.post()
				return &tok{rune: SEP}
			case 0:
				l.err("unterminated comment")
				return &tok{rune: -1}
			}
		}
	case 0:
		l.pos--
		return &tok{rune: -1}
	default:
		panic(todo("%v: %#+U", l.position(), l.c()))
	}
}

func isDigit(c byte) bool   { return c >= '0' && c <= '9' }
func isIdFirst(c byte) bool { return c >= 'a' && c <= 'z' || c >= 'A' && c <= 'Z' || c == '_' }
func isIdNext(c byte) bool  { return isIdFirst(c) || c >= '0' && c <= '9' }

func (l *lexer) isSep(c byte) bool {
	switch c {
	case '\n':
		l.file.AddLine(l.si + 1)
		fallthrough
	case ' ':
		return true
	}

	return false
}

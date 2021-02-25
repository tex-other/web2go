%{
// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

%}

%union {
	tok			tok
	node			node
}

%token
	/*yy:token "%c"		*/	IDENTIFIER	"identifier"
	/*yy:token "%d"		*/	INT_LITERAL     "integer literal"
	/*yy:token "%d.123"	*/	REAL_LITERAL    "real literal"
	/*yy:token "%q"		*/	STR_LITERAL     "string literal"

	SEP             "separator"

	AND             "and"
	ARRAY           "array"
	ASSIGN          ":="
	BEGIN           "begin"
	BOOLEAN         "boolean"
	CASE            "case"
	CONST           "const"
	DD              ".."
	DIV             "div"
	DO              "do"
	ELSE            "else"
	END             "end"
	FALSE           "false"
	FILE            "file"
	FOR             "for"
	FORWARD         "forward"
	FUNCTION        "function"
	GE              ">="
	GOTO            "goto"
	IF              "if"
	INTEGER         "integer"
	LABEL           "label"
	LE              "<="
	NOT             "not"
	OF              "of"
	OR              "or"
	PACKED          "packed"
	PROCEDURE       "procedure"
	PROGRAM         "program"
	REAL            "real"
	RECORD          "record"
	REPEAT          "repeat"
	THEN            "then"
	TO              "to"
	TRUE            "true"
	VAR             "var"
	WHILE           "while"

%start program

%%

program:
	Program
	{
		panic(todo(""))
	}

Program:
	"program" IDENTIFIER

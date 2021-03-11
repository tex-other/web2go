// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"go/token"
	"runtime"
	"strings"

	"modernc.org/strutil"
)

var (
	universe = &scope{
		isUniverse: true,
		m: map[string]node{
			"abs": &functionDeclaration{
				functionHeading: &functionHeading{args: []typ{aReal}},
				typ:             aReal,
			},
			"boolean": &boolean{},
			"break": &procedureDeclaration{
				procedureHeading: &procedureHeading{args: []typ{aFile}},
			},
			"break_in": &procedureDeclaration{
				procedureHeading: &procedureHeading{args: []typ{aFile, aBoolean}},
			},
			"char": &char{},
			"chr": &functionDeclaration{
				functionHeading: &functionHeading{args: []typ{aInteger}},
				typ:             aChar,
			},
			"close": &procedureDeclaration{
				procedureHeading: &procedureHeading{args: []typ{aFile}},
			},
			"eof": &functionDeclaration{
				functionHeading: &functionHeading{args: []typ{aFile}},
				typ:             aBoolean,
			},
			"eoln": &functionDeclaration{
				functionHeading: &functionHeading{args: []typ{aFile}},
				typ:             aBoolean,
			},
			"erstat": &functionDeclaration{
				functionHeading: &functionHeading{args: []typ{aFile}},
				typ:             aInteger,
			},
			"false": &constantDefinition{constant: &constant{literal: newBooleanLiteral(false)}},
			"get": &procedureDeclaration{
				procedureHeading: &procedureHeading{args: []typ{aFile}},
			},
			"integer": &integer{},
			"odd": &functionDeclaration{
				functionHeading: &functionHeading{args: []typ{aInteger}},
				typ:             aBoolean,
			},
			"pasjumpout": &procedureDeclaration{
				procedureHeading: &procedureHeading{},
			},
			"put": &procedureDeclaration{
				procedureHeading: &procedureHeading{args: []typ{aFile}},
			},
			"read": &procedureDeclaration{
				procedureHeading: &procedureHeading{},
				isRead:           true,
			},
			"read_ln": &procedureDeclaration{
				procedureHeading: &procedureHeading{},
				isRead:           true,
			},
			"real": &real{},
			"reset": &procedureDeclaration{
				procedureHeading: &procedureHeading{args: []typ{aFile, aString, aString}},
				isResetOrRewrite: true,
			},
			"rewrite": &procedureDeclaration{
				procedureHeading: &procedureHeading{args: []typ{aFile, aString, aString}},
				isResetOrRewrite: true,
			},
			"round": &functionDeclaration{
				functionHeading: &functionHeading{args: []typ{aReal}},
				typ:             aInteger,
			},
			"true": &constantDefinition{constant: &constant{literal: newBooleanLiteral(true)}},
			"write": &procedureDeclaration{
				procedureHeading: &procedureHeading{},
				isWrite:          true,
			},
			"write_ln": &procedureDeclaration{
				procedureHeading: &procedureHeading{},
				isWrite:          true,
			},
		},
	}
)

func parse(t *task, b []byte, name string) (*program, error) {
	p, err := newParser(t, b, name)
	if err != nil {
		return nil, err
	}

	n := p.program()
	if len(p.errs) != 0 {
		return nil, p.errList()
	}

	if n == nil {
		return nil, fmt.Errorf("internal error: parser returns nil but no error")
	}

	return n, nil
}

type parser struct {
	*scanner
	task     *task
	tok      *tok // current token if not nil and ungetBuf is nil
	ungetBuf *tok // current token if not nil
	universe *scope

	debug          bool
	overflowCheck  bool
	rangeCheck     bool
	seenDirectives bool
}

func newParser(t *task, b []byte, name string) (*parser, error) {
	s, err := newScanner(b, name)
	if err != nil {
		return nil, err
	}

	s.allErrors = t.e
	return &parser{
		scanner:  s,
		task:     t,
		universe: universe,
	}, nil
}

func (p *parser) err(n node, msg string, args ...interface{}) {
	if !p.allErrors && len(p.errs) == 10 {
		return
	}

	pos := fmt.Sprintf("%v: ", n.Position())
	msg = fmt.Sprintf(pos+msg, args...)
	if strings.Contains(msg, "internal error") {
		msg += fmt.Sprintf(" (%v)", origin(2))
	}
	p.errs = append(p.errs, msg)
}

func (p *parser) c() (r *tok) {
	for {
		tok := p.c0()
		if tok.ch != SEP {
			return tok
		}

		if strings.HasPrefix(tok.src, "{$") {
			if p.seenDirectives {
				p.err(tok, "multiple directive instances not supported")
			}
			p.directives(tok)
			p.seenDirectives = true
		}

		p.shift()
	}
}

func (p *parser) directives(tok *tok) {
	if !strings.HasPrefix(tok.src, "{$") || !strings.HasSuffix(tok.src, "}") {
		p.err(tok, "invalid directive format: %s", tok.src)
		return
	}

	a := strings.Split(tok.src[len("{$"):len(tok.src)-len("}")], ",")
	for _, v := range a {
		switch v {
		case "C+":
			p.rangeCheck = true
		case "C-":
			p.rangeCheck = false
		case "A+":
			p.overflowCheck = true
		case "A-":
			p.overflowCheck = false
		case "D+":
			p.debug = true
		case "D-":
			p.debug = false
		default:
			p.err(tok, "unsupported directive: %q", v)
		}
	}
}

// 	tok		ungetBuf	action
//	---		--------	------
//	nil		nil		p.tok = p.scan(); return p.tok
//	nil		non-nil		return p.ungetBuf
//	non-nil		nil		return p.tok
//	non-nil		non-nil		return p.ungetBuf
func (p *parser) c0() (r *tok) {
	if p.ungetBuf != nil {
		return p.ungetBuf
	}

	if p.tok != nil {
		return p.tok
	}

	p.tok = p.scan()
	return p.tok
}

// 	tok		ungetBuf	action
//	---		--------	------
//	nil		nil		invalid
//	nil		non-nil		r = p.ungetBuf; p.ungetBuf = nil; return r
//	non-nil		nil		r = p.tok; p.tok = nil; return r
//	non-nil		non-nil		r = p.ungetBuf; p.ungetBuf = nil; return r
func (p *parser) shift() (r *tok) {
	if p.ungetBuf != nil {
		r = p.ungetBuf
		p.ungetBuf = nil
		return r
	}

	if p.tok != nil {
		r = p.tok
		p.tok = nil
		return r
	}

	panic(todop(p, "internal error"))
}

// 	tok		ungetBuf	action
//	---		--------	------
//	nil		nil		p.ungetBuf = tok
//	nil		non-nil		p.tok = p.ungetBuf; p.ungetBuf = tok
//	non-nil		nil		p.ungetBuf = tok
//	non-nil		non-nil		invalid
func (p *parser) unget(tok *tok) {
	if p.ungetBuf == nil {
		p.ungetBuf = tok
		return
	}

	if p.tok == nil {
		p.tok = p.ungetBuf
		p.ungetBuf = tok
		return
	}

	panic(todop(p, "internal error"))
}

func (p *parser) must(ch ch) (tok *tok) {
	if tok = p.c(); tok.ch == ch {
		return tok
	}

	p.err(tok, "%s, expected %s (%v)", tok, ch.str(), origin(3))
	return nil
}

func (p *parser) mustShift(ch ch) *tok {
	tok := p.must(ch)
	p.shift()
	return tok
}

func todop(p *parser, s string, args ...interface{}) string {
	switch {
	case s == "":
		s = fmt.Sprint(args...)
	default:
		s = fmt.Sprintf(s, args...)
	}
	return fmt.Sprintf("%v: %v\n%s", origin(2), p.errList(), s)
}

type scope struct {
	m      map[string]node
	parent *scope

	isTLD      bool
	isUniverse bool
}

func (s *scope) declare(p *parser, n node, nm string) {
	nm = strings.ToLower(nm) // Pascal identifiers are not case sensitive.
	m := s.m
	if m == nil {
		m = map[string]node{}
		s.m = m
	}
	switch ex := m[nm].(type) {
	case nil:
		m[nm] = n
	case *procedureDeclaration:
		if new, ok := n.(*procedureDeclaration); ok {
			if ex.forward != nil && new.isCompatible(ex) {
				m[nm] = n
				return
			}
		}
	case *functionDeclaration:
		if new, ok := n.(*functionDeclaration); ok {
			if ex.forward != nil && new.isCompatible(ex) {
				m[nm] = n
				return
			}
		}
	default:
		p.err(n, "%s redeclared, previous declaration at %v:", nm, ex.Position())
	}
}

func (s *scope) get(nm string) node { return s.m[strings.ToLower(nm)] }

func (s *scope) find(nm string) (*scope, node) {
	for s != nil {
		if n := s.get(nm); n != nil {
			return s, n
		}

		s = s.parent
	}
	return nil, nil
}

// Program = ProgramHeading ";" Block "." .
type program struct {
	*scope
	*programHeading
	semi *tok
	*block
	dot *tok
}

func (p *parser) program() *program {
	var r program
	r.scope = &scope{parent: p.universe}
	r.scope.isTLD = true
	r.programHeading = p.programHeading()
	r.semi = p.mustShift(';')
	r.block = p.block(r.scope)
	r.dot = p.mustShift('.')
	return &r
}

// ProgramHeading = "program" Identifier [ ProgramParameterList ] .
type programHeading struct {
	program *tok
	ident   *tok
	*programParameterList
}

func (p *parser) programHeading() *programHeading {
	return &programHeading{
		p.mustShift(PROGRAM),
		p.mustShift(IDENTIFIER),
		p.programParameterList(),
	}
}

type programParameterList struct{}

// ProgramParameterList = "(" IdentifierList ")" .
func (p *parser) programParameterList() *programParameterList {
	if p.c().ch == '(' {
		p.err(p.c(), "ProgramParameterList not supported")
		p.shift()
	}
	return nil
}

type block struct {
	*labelDeclarationPart
	*constantDefinitionPart
	*typeDefinitionPart
	*variableDeclarationPart
	list []*procedureAndFunctionDeclarationPart
	*compoundStatement
}

// Block = LabelDeclarationPart
//	ConstantDefinitionPart
//	TypeDefinitionPart
//	VariableDeclarationPart
//	ProcedureAndFunctionDeclarationPart
//	StatementPart .
func (p *parser) block(s *scope) *block {
	return &block{
		p.labelDeclarationPart(s),
		p.constantDefinitionPart(s),
		p.typeDefinitionPart(s),
		p.variableDeclarationPart(s),
		p.procedureAndFunctionDeclarationPart(s),
		p.compoundStatement(s, true),
	}
}

// CompoundStatement = "begin" StatementSequence "end" .
type compoundStatement struct {
	begin *tok
	list  []*statement
	end   *tok
}

func (p *parser) compoundStatement(s *scope, blockTop bool) *compoundStatement {
	r := &compoundStatement{
		p.mustShift(BEGIN),
		p.statementList(s),
		p.mustShift(END),
	}
	if blockTop && len(r.list) != 0 {
		r.list[len(r.list)-1].isLastInBlock = true
	}
	return r
}

// StatementSequence = Statement { ";" Statement} .
func (p *parser) statementList(s *scope) (r []*statement) {
	r = []*statement{p.statement(s)}
	for p.c().ch == ';' {
		p.shift()
		switch p.c().ch {
		case UNTIL, ELSE, END, OTHERS:
			return r
		}

		r = append(r, p.statement(s))
	}
	return r
}

// Statement = [ Label ":" ] ( SimpleStatement | StructuredStatement ) .
type statement struct {
	label *tok
	*simpleStatement
	*structuredStatmenent

	isIf          bool
	isLastInBlock bool
}

func (p *parser) statement(s *scope) *statement {
	var label *tok
	if p.c().ch == INT_LITERAL {
		label = p.shift()
		p.mustShift(':')
	}
	switch p.c().ch {
	case BEGIN, IF, WHILE, REPEAT, FOR, WITH, CASE:
		r := &statement{
			label:                label,
			structuredStatmenent: p.structuredStatmenent(s),
		}
		r.isIf = r.structuredStatmenent.isIf
		return r
	default:
		return &statement{
			label:           label,
			simpleStatement: p.simpleStatement(s),
		}
	}
}

// StructuredStatement = CompoundStatement | ConditionalStatement
//	| RepetitiveStatement | WithStatement .
type structuredStatmenent struct {
	*repetitiveStatement
	*compoundStatement
	*conditionalStatement

	isIf bool
}

func (p *parser) structuredStatmenent(s *scope) *structuredStatmenent {
	switch p.c().ch {
	case FOR, WHILE, REPEAT:
		return &structuredStatmenent{repetitiveStatement: p.repetitiveStatement(s)}
	case BEGIN:
		return &structuredStatmenent{compoundStatement: p.compoundStatement(s, false)}
	case CASE, IF:
		r := &structuredStatmenent{conditionalStatement: p.conditionalStatement(s)}
		r.isIf = r.conditionalStatement.ifStatement != nil
		return r
	}

	p.err(p.c(), "unexpected %q, expected compound statement or conditional statement or repetitive statement or with statement", p.c().src)
	p.shift()
	return nil
}

// conditionalStatement = IfStatement | CaseStatement .
type conditionalStatement struct {
	*caseStatement
	*ifStatement
}

func (p *parser) conditionalStatement(s *scope) *conditionalStatement {
	switch p.c().ch {
	case CASE:
		return &conditionalStatement{caseStatement: p.caseStatement(s)}
	case IF:
		return &conditionalStatement{ifStatement: p.ifStatement(s)}
	}

	p.err(p.c(), "unexpected %q, expected if statement or case statement", p.c().src)
	p.shift()
	return nil
}

// IfStatement = "if" BooleanExpression "then" Statement
//	[ "else" Statement ] .
type ifStatement struct {
	if_ *tok
	*expression
	then *tok
	*statement
	else_         *tok
	elseStatement *statement
}

func (p *parser) ifStatement(s *scope) *ifStatement {
	r := &ifStatement{
		if_:        p.mustShift(IF),
		expression: p.expression(s),
		then:       p.mustShift(THEN),
		statement:  p.statement(s),
	}
	if p.c().ch == ELSE {
		r.else_ = p.shift()
		r.elseStatement = p.statement(s)
	}
	return r
}

// CaseStatement = "case" CaseIndex "of"
//	Case { ";" Case } [";"]
//	"end"
type caseStatement struct {
	case_ *tok
	*expression
	of   *tok
	list []*case_
	semi *tok
	end  *tok
}

func (p *parser) caseStatement(s *scope) *caseStatement {
	return &caseStatement{
		p.mustShift(CASE),
		p.expression(s),
		p.mustShift(OF),
		p.caseList(s),
		p.semiOpt(),
		p.mustShift(END),
	}
}

func (p *parser) semiOpt() *tok {
	if p.c().ch == ';' {
		return p.shift()
	}

	return nil
}

func (p *parser) caseList(s *scope) (r []*case_) {
	r = []*case_{p.case_(s)}
	for p.c().ch == ';' {
		semi := p.shift()
		if p.c().ch == END {
			p.unget(semi)
			return r
		}

		r = append(r, p.case_(s))
	}
	return r
}

// Case = Constant { "," Constant } ":" Statement |
//	"else" Statement .
type case_ struct {
	list   []*constant
	colon  *tok
	others *tok
	*statement
}

func (p *parser) case_(s *scope) *case_ {
	if p.c().ch == OTHERS { // others: == else
		return &case_{
			others:    p.shift(),
			colon:     p.mustShift(':'),
			statement: p.statement(s),
		}
	}

	return &case_{
		list:      p.constantList(s),
		colon:     p.mustShift(':'),
		statement: p.statement(s),
	}
}

func (p *parser) constantList(s *scope) (r []*constant) {
	r = []*constant{p.constant(s)}
	for p.c().ch == ',' {
		p.shift()
		r = append(r, p.constant(s))
	}
	return r
}

// RepetitiveStatement = WhileStatement | RepeatStatement | ForStatement .
type repetitiveStatement struct {
	*forStatement
	*whileStatement
	*repeatStatement
}

func (p *parser) repetitiveStatement(s *scope) *repetitiveStatement {
	switch p.c().ch {
	case FOR:
		return &repetitiveStatement{forStatement: p.forStatement(s)}
	case WHILE:
		return &repetitiveStatement{whileStatement: p.whileStatement(s)}
	case REPEAT:
		return &repetitiveStatement{repeatStatement: p.repeatStatement(s)}
	}

	p.err(p.c(), "unexpected %q, expected while statement or repeat statement or for statement", p.c().src)
	p.shift()
	return nil
}

// RepeatStatement = "repeat" Statement "until" Expression .
type repeatStatement struct {
	repeat *tok
	list   []*statement
	until  *tok
	*expression
}

func (p *parser) repeatStatement(s *scope) *repeatStatement {
	return &repeatStatement{
		p.mustShift(REPEAT),
		p.statementList(s),
		p.mustShift(UNTIL),
		p.expression(s),
	}
}

// WhileStatement = "while" BooleanExpression "do" Statement .
type whileStatement struct {
	while *tok
	*expression
	do *tok
	*statement
}

func (p *parser) whileStatement(s *scope) *whileStatement {
	return &whileStatement{
		p.mustShift(WHILE),
		p.expression(s),
		p.mustShift(DO),
		p.statement(s),
	}
}

// ForStatement = "for" ControlVariable ":=" InitialValue ( "to" | "downto" ) FinalValue "do" Statement .
type forStatement struct {
	for_ *tok
	*identifier
	assign     *tok
	initial    *expression
	toOrDownto *tok
	final      *expression
	do         *tok
	*statement
}

func (p *parser) forStatement(s *scope) *forStatement {
	return &forStatement{
		p.mustShift(FOR),
		p.identifier(s, false),
		p.mustShift(ASSIGN),
		p.expression(s),
		p.toOrDownto(),
		p.expression(s),
		p.mustShift(DO),
		p.statement(s),
	}
}

type identifier struct {
	*tok
	def   node
	scope *scope
	typ
}

func (p *parser) identifier(s *scope, useVar bool) *identifier {
	r := &identifier{tok: p.mustShift(IDENTIFIER)}
	if r.tok == nil {
		return r
	}

	if r.scope, r.def = s.find(r.tok.src); r.def == nil {
		p.err(r.tok, "undefined: %s", r.tok.src)
	}
	switch x := r.def.(type) {
	case *char, *integer, *real, *boolean, *procedureDeclaration:
		// nop
	case *typeDefinition:
		r.typ = x.typ
	case *variableDeclaration:
		if useVar && !x.scope.isTLD {
			x.used[r.tok.src] = struct{}{}
		}
		r.typ = x.typ
	case *functionDeclaration:
		r.typ = x.typ
	case *constantDefinition:
		r.typ = x.literal.typ()
	case *valueParameterSpecification:
		r.typ = x.typ
	case *variableParameterSpecification:
		r.typ = x.typ
	}
	return r
}

func (p *parser) toOrDownto() *tok {
	switch p.c().ch {
	case TO, DOWNTO:
		return p.shift()
	}

	p.err(p.c(), "unexpected %q, expected 'to' or 'downto'", p.c().src)
	p.shift()
	return nil
}

// SimpleStatement = EmptyStatement | AssignmentStatement | ProcedureStatement | GotoStatement .
type simpleStatement struct {
	*assignmentStatement
	*procedureStatement
	*gotoStatement

	isEmpty bool
}

func (p *parser) simpleStatement(s *scope) *simpleStatement {
	switch p.c().ch {
	case ';', END:
		return &simpleStatement{isEmpty: true}
	case GOTO:
		return &simpleStatement{gotoStatement: p.gotoStatement()}
	case IDENTIFIER:
		id := p.shift()
		switch p.c().ch {
		case '(', ';', ELSE, END, UNTIL:
			p.unget(id)
			return &simpleStatement{procedureStatement: p.procedureStatement(s)}
		}

		p.unget(id)
		return &simpleStatement{assignmentStatement: p.assignmentStatement(s)}
	}

	p.err(p.c(), "unexpected %q, expected simple type or structured type or pointer type", p.c().src)
	p.shift()
	return nil
}

// GotoStatement = "goto" Label .
type gotoStatement struct {
	goto_ *tok
	label *tok
}

func (p *parser) gotoStatement() *gotoStatement {
	return &gotoStatement{
		p.mustShift(GOTO),
		p.mustShift(INT_LITERAL),
	}
}

// ProcedureStatement = ProcedureIdentifier [ ActualParameterList | WriteParameterList ] .
type procedureStatement struct {
	*identifier
	list []*arg
}

func (p *parser) procedureStatement(s *scope) *procedureStatement {
	c0 := p.c()
	r := &procedureStatement{identifier: p.identifier(s, true)}
	var args []typ
	var isRead, isWrite bool
	switch x := r.identifier.def.(type) {
	case nil:
		// handled in p.identifier
	case *procedureDeclaration:
		args = x.procedureHeading.args
		isRead = x.isRead
		isWrite = x.isWrite
	default:
		p.err(c0, "not a procedure: %s", r.identifier.src)
	}
	r.list = p.argList(s, args, isRead, isWrite)
	return r
}

func (p *parser) argList(s *scope, args []typ, isRead, isWrite bool) (r []*arg) {
	if p.c().ch != '(' {
		return nil
	}

	p.shift()
	ix := 0
	r = []*arg{p.arg(s, args, ix, isRead, isWrite)}
	for p.c().ch == ',' {
		p.shift()
		ix++
		r = append(r, p.arg(s, args, ix, isRead, isWrite))
	}
	p.mustShift(')')
	return r
}

type arg struct {
	*expression
	colon  *tok
	width  *expression
	colon2 *tok
	width2 *expression
}

func (p *parser) arg(s *scope, args []typ, ix int, isRead, isWrite bool) *arg {
	c0 := p.c()
	r := &arg{expression: p.expression(s)}
	switch {
	case isRead, isWrite:
		// nop
	default:
		p.checkArg(c0, r.expression.typ, args, ix)
	}
	if p.c().ch == ':' {
		r.colon = p.shift()
		r.width = p.expression(s)
		if p.c().ch == ':' {
			r.colon2 = p.shift()
			r.width2 = p.expression(s)
		}
	}
	return r
}

func (p *parser) checkArg(n node, rhs typ, args []typ, ix int) {
	if ix >= len(args) {
		p.err(n, "too many arguments")
		return
	}

	lhs := args[ix]
	if lhs == nil {
		p.err(n, "formal parameter #%d type not resolved", ix)
		return
	}

	if !lhs.canBeAssignedFrom(rhs) {
		p.err(n, "cannot assign %s to %s", rhs, lhs)
	}
}

// AssignmentStatement = ( Variable | FunctionIdentifier ) ":=" Expression .
type assignmentStatement struct {
	*variable
	assign *tok
	*expression
}

func (p *parser) assignmentStatement(s *scope) *assignmentStatement {
	r := &assignmentStatement{
		p.variable(s, false),
		p.mustShift(ASSIGN),
		p.expression(s),
	}
	if lhs, rhs := r.variable.typ, r.expression.typ; !lhs.canBeAssignedFrom(rhs) {
		p.err(r.assign, "cannot assign %s to %s", rhs, lhs)
	}
	return r
}

// Expression = SimpleExression [ RelationalOperator SimpleExression ] .
type expression struct {
	*simpleExpression
	relOp *tok
	rhs   *simpleExpression
	typ
	literal
	*identifier

	isConst bool
}

func (p *parser) expression(s *scope) *expression {
	c0 := p.c()
	r := &expression{simpleExpression: p.simpleExpression(s)}
	r.typ = r.simpleExpression.typ
	r.literal = r.simpleExpression.literal
	r.isConst = r.simpleExpression.isConst
	r.identifier = r.simpleExpression.identifier
	switch p.c().ch {
	case '=', NE, '<', LE, '>', GE, IN:
		r.relOp = p.shift()
		r.rhs = p.simpleExpression(s)
		r.typ = p.relOp(r.relOp, r.typ, r.rhs.typ, r.relOp.ch)
		r.literal = nil
		r.isConst = true
		r.identifier = nil
	}
	if r.typ == nil {
		p.err(c0, "expression type not resolved")
	}
	return r
}

func (p *parser) relOp(n node, lhs, rhs typ, op ch) (r typ) {
	if lhs == nil || rhs == nil {
		return lhs
	}

	x, ok := lhs.(relator)
	if !ok {
		p.err(n, "invalid relation operator lhs operand type: %s", lhs)
		return lhs
	}

	y, ok := rhs.(relator)
	if !ok {
		p.err(n, "invalid relation operator rhs operand type: %s", rhs)
		return lhs
	}

	var err error
	switch op {
	case '=':
		r, err = x.eq(y)
	case NE:
		r, err = x.ne(y)
	case '<':
		r, err = x.lt(y)
	case LE:
		r, err = x.le(y)
	case '>':
		r, err = x.gt(y)
	case GE:
		r, err = x.ge(y)
	case IN:
		r, err = x.in(y)
	default:
		err = fmt.Errorf("internal error: %s", op)
	}

	if err != nil {
		p.err(n, "%s", err)
	}
	return r
}

type termListItem struct {
	mulOp *tok
	*factor
}

// Term = Factor { MultiplyingOperator Factor } .
type term struct {
	*factor
	list []termListItem
	typ
	literal
	*identifier

	isConst bool
}

// a*b*c = (a*b)*c
func (p *parser) term(s *scope) *term {
	r := &term{factor: p.factor(s)}
	r.typ = r.factor.typ
	r.literal = r.factor.literal
	r.isConst = r.factor.unsignedConstant != nil
	r.identifier = r.factor.identifier
	for {
		switch p.c().ch {
		case '*', '/', DIV, MOD, AND:
			item := termListItem{
				p.shift(),
				p.factor(s),
			}
			r.typ = p.mulOp(item.mulOp, r.typ, item.factor.typ, item.mulOp.ch)
			r.list = append(r.list, item)
			r.literal = nil
			r.isConst = false
			r.identifier = nil
		default:
			return r
		}
	}
}

func (p *parser) mulOp(n node, lhs, rhs typ, op ch) (r typ) {
	if lhs == nil || rhs == nil {
		return lhs
	}

	x, ok := lhs.(multiplier)
	if !ok {
		p.err(n, "invalid multiplying operator lhs operand type: %s", lhs)
		return lhs
	}

	y, ok := rhs.(multiplier)
	if !ok {
		p.err(n, "invalid multiplying operator rhs operand type: %s", rhs)
		return lhs
	}

	var err error
	switch op {
	case '*':
		r, err = x.mul(y)
	case '/':
		r, err = x.div(y)
	case DIV:
		r, err = x.idiv(y)
	case MOD:
		r, err = x.mod(y)
	case AND:
		r, err = x.and(y)
	default:
		err = fmt.Errorf("internal error: %s", op)
	}

	if err != nil {
		p.err(n, "%s", err)
	}
	return r
}

// Factor = UnsignedConstant | BoundIdentifier | Variable
//	| SetConstructor | FunctionDesignator |
//	"not" factor | "(" Expression ")" .
type factor struct {
	*unsignedConstant
	*functionDesignator
	*variable
	*expression
	not *factor
	typ
	literal
	*identifier

	isConst bool
}

func (p *parser) factor(s *scope) *factor {
	c0 := p.c()
	switch p.c().ch {
	case INT_LITERAL, STR_LITERAL, REAL_LITERAL:
		r := &factor{unsignedConstant: p.unsignedConstant(s)}
		r.typ = r.unsignedConstant.literal.typ()
		r.literal = r.unsignedConstant.literal
		return r
	case IDENTIFIER:
		id := p.shift()
		if p.c().ch == '(' {
			p.unget(id)
			r := &factor{functionDesignator: p.functionDesignator(s)}
			r.typ = r.functionDesignator.typ
			return r
		}

		switch _, def := s.find(id.src); x := def.(type) {
		case *variableDeclaration:
			p.unget(id)
			r := &factor{variable: p.variable(s, true)}
			r.typ = r.variable.typ
			r.identifier = r.variable.identifier
			return r
		case *constantDefinition:
			p.unget(id)
			r := &factor{unsignedConstant: p.unsignedConstant(s)}
			r.typ = x.constant.literal.typ()
			r.isConst = true
			return r
		case *valueParameterSpecification:
			p.unget(id)
			r := &factor{variable: p.variable(s, false)}
			r.typ = r.variable.typ
			r.identifier = r.variable.identifier
			return r
		case *variableParameterSpecification:
			p.unget(id)
			r := &factor{variable: p.variable(s, false)}
			r.typ = r.variable.typ
			r.identifier = r.variable.identifier
			return r
		case *functionDeclaration:
			p.unget(id)
			r := &factor{functionDesignator: p.functionDesignator(s)}
			r.typ = r.functionDesignator.typ
			return r
		default:
			p.err(c0, "internal error: %T", x)
			return nil
		}
	case '(':
		p.shift()
		r := &factor{expression: p.expression(s)}
		r.typ = r.expression.typ
		p.mustShift(')')
		return r
	case NOT:
		not := p.shift()
		r := &factor{not: p.factor(s)}
		r.typ = p.not(not, r.not.typ)
		return r
	}

	p.err(c0, "unexpected %q, expected unsigned constant or varible or set constructor or 'not' or '( expression ')'", p.c().src)
	p.shift()
	return nil
}

func (p *parser) not(n node, op typ) (r typ) {
	if op == nil {
		return op
	}

	x, ok := op.(negator)
	if !ok {
		p.err(n, "invalid not operator operand type: %s", op)
		return op
	}

	var err error
	if r, err = x.not(); err != nil {
		p.err(n, "%s", err)
	}
	return r
}

// FunctionDesignator = FunctionIdentifier [ ActualParameterList ] .
type functionDesignator struct {
	*identifier
	list []*expression
	args []typ
	typ

	replace string
}

func (p *parser) functionDesignator(s *scope) *functionDesignator {
	c0 := p.c()
	r := &functionDesignator{identifier: p.identifier(s, true)}
	var args []typ
	switch x := r.identifier.def.(type) {
	case nil:
		// handled in p.identifier
	case *functionDeclaration:
		r.typ = x.typ
		args = x.functionHeading.args
		r.args = x.functionHeading.args
	default:
		p.err(c0, "not a function: %s", r.identifier.src)
	}
	r.list = p.actualParameterList(s, args)
	switch x := r.identifier.def.(type) {
	case nil:
		// handled in p.identifier
	case *functionDeclaration:
		if r.identifier.scope.isUniverse {
			switch r.identifier.src {
			case "abs":
				if len(r.list) != 0 {
					if _, ok := r.list[0].typ.(ordinal); ok {
						r.replace = "iabs"
						r.args = []typ{aInteger}
						r.typ = aInteger
						return r
					}
				}
			}
		}

		r.typ = x.typ
	default:
		p.err(c0, "not a function: %s", r.identifier.src)
	}
	return r
}

func (p *parser) actualParameterList(s *scope, args []typ) (r []*expression) {
	if p.c().ch != '(' {
		return nil
	}

	p.shift()
	r = p.expressionList(s, args, true)
	p.mustShift(')')
	return r
}

// UnsignedConstant = UnsignedNumber | CharacterString | ConstantIdentifier | "nil" .
type unsignedConstant struct {
	*unsignedNumber
	str *tok
	*identifier
	literal
}

func (p *parser) unsignedConstant(s *scope) *unsignedConstant {
	c0 := p.c()
	switch c0.ch {
	case INT_LITERAL, REAL_LITERAL:
		r := &unsignedConstant{unsignedNumber: p.unsignedNumber()}
		r.literal = r.unsignedNumber.literal
		return r
	case STR_LITERAL:
		r := &unsignedConstant{str: p.shift()}
		var err error
		if r.literal, err = newLiteralFromString(goStringFromPascalString(r.str.src)); err != nil {
			p.err(c0, "%s", err)
		}
		return r
	case IDENTIFIER:
		r := &unsignedConstant{identifier: p.identifier(s, false)}
		switch x := r.identifier.def.(type) {
		case *constantDefinition:
			switch y := x.literal.typ().(type) {
			case *boolean, *integer:
				r.literal = x.literal
			case *array:
				if y.isString {
					r.literal = x.literal
					break
				}

				p.err(c0, "internal error: %T", y)
			default:
				p.err(c0, "internal error: %T", y)
			}
		default:
			p.err(c0, "internal error: %T", x)
		}
		return r
	}

	p.err(c0, "unexpected %q, expected unsigned number or character string or identifier or 'nil'", p.c().src)
	p.shift()
	return nil
}

// UnsignedNumber = UnsignedInteger | UnsignedReal .
type unsignedNumber struct {
	int  *tok
	real *tok
	literal
}

func (p *parser) unsignedNumber() *unsignedNumber {
	var err error
	switch p.c().ch {
	case INT_LITERAL:
		r := &unsignedNumber{int: p.shift()}
		if r.literal, err = newIntegerLiteralFromString(r.int.src); err != nil {
			p.err(r.int, "%s", err)
		}
		return r
	case REAL_LITERAL:
		r := &unsignedNumber{real: p.shift()}
		if r.literal, err = newRealLiteralFromString(r.real.src); err != nil {
			p.err(r.int, "%s", err)
		}
		return r
	}

	p.err(p.c(), "unexpected %q, expected unsigned integer or unsigned real", p.c().src)
	p.shift()
	return nil
}

type simpleExpressionListItem struct {
	addOp *tok
	*term
}

// SimpleExpression = [ Sign ] Term { AddingOperator Term } .
type simpleExpression struct {
	sign *tok
	*term
	list []simpleExpressionListItem
	typ
	literal
	*identifier

	isConst bool
}

func (p *parser) simpleExpression(s *scope) *simpleExpression {
	c0 := p.c()
	sign := p.sign()
	r := &simpleExpression{
		sign: sign,
		term: p.term(s),
	}
	r.literal = r.term.literal
	r.typ = r.term.typ
	r.isConst = r.term.isConst
	r.identifier = r.term.identifier
	if sign != nil && sign.src == "-" {
		r.typ = p.inverse(c0, r.typ)
	}
	for {
		switch p.c().ch {
		case '+', '-', OR:
			item := simpleExpressionListItem{
				p.shift(),
				p.term(s),
			}
			r.typ = p.addOp(item.addOp, r.typ, item.term.typ, item.addOp.ch)
			r.list = append(r.list, item)
			r.literal = nil
			r.isConst = false
			r.term.identifier = nil
		default:
			return r
		}
	}
}

func (p *parser) inverse(n node, op typ) (r typ) {
	if op == nil {
		return op
	}

	x, ok := op.(invertor)
	if !ok {
		p.err(n, "additive inverse of a wrong type: %v", op)
		return op
	}

	var err error
	if r, err = x.inverse(); err != nil {
		p.err(n, "%s", err)
	}
	return r
}

func (p *parser) addOp(n node, lhs, rhs typ, op ch) (r typ) {
	if lhs == nil || rhs == nil {
		return lhs
	}

	x, ok := lhs.(adder)
	if !ok {
		p.err(n, "invalid adding operator lhs operand type: %s", lhs)
		return lhs
	}

	y, ok := rhs.(adder)
	if !ok {
		p.err(n, "invalid adding operator rhs operand type: %s", rhs)
		return lhs
	}

	var err error
	switch op {
	case '+':
		r, err = x.add(y)
	case '-':
		r, err = x.sub(y)
	case OR:
		r, err = x.or(y)
	default:
		err = fmt.Errorf("internal error: %s", op)
	}
	if err != nil {
		p.err(n, "%s", err)
	}
	return r
}

func (p *parser) expressionList(s *scope, args []typ, isCall bool) (r []*expression) {
	c0 := p.c()
	var ix int
	r = []*expression{p.expression(s)}
	if isCall {
		p.checkArg(c0, r[0].typ, args, ix)
	}
	for p.c().ch == ',' {
		p.shift()
		expr := p.expression(s)
		ix++
		if isCall {
			p.checkArg(c0, expr.typ, args, ix)
		}
		r = append(r, expr)
	}
	return r
}

// Variable = EntireVariable | ComponentVariable |
//	IdentifiedVariable | BufferVariable .
type variable struct {
	*identifier
	*componentVariable
	deref *variable
	typ
	*field
}

func (p *parser) variable(s *scope, useVar bool) (r *variable) {
	switch p.c().ch {
	case IDENTIFIER:
		r = &variable{identifier: p.identifier(s, useVar)}
		switch x := r.identifier.def.(type) {
		case nil:
			// handled in p.identifier
		case *variableDeclaration:
			if useVar && !x.scope.isTLD {
				x.used[r.identifier.src] = struct{}{}
			}
			r.typ = x.typ
		case *valueParameterSpecification:
			r.typ = x.typ
		case *functionDeclaration:
			r.typ = x.typ
		case *variableParameterSpecification:
			r.typ = x.typ
		default:
			p.err(r.identifier.tok, "not a variable: %s", r.identifier.src)
		}
		for {
			switch p.c().ch {
			case '[':
				r = &variable{componentVariable: p.componentVariable(s, r)}
				r.typ = r.componentVariable.typ
			case '.':
				r = &variable{componentVariable: p.componentVariable(s, r)}
				r.typ = r.componentVariable.typ
				r.field = r.componentVariable.field
			case '^':
				elem := typ(aInteger)
				switch x := r.typ.(type) {
				case *file:
					elem = x.component
				default:
					p.err(p.c(), "expected file type: %s", r.typ)
				}
				p.shift()
				r = &variable{deref: r}
				r.typ = elem
			default:
				return r
			}
		}
	}

	p.err(p.c(), "unexpected %q, expected variable identifier or component variable or identified variable of buffer variable", p.c().src)
	p.shift()
	return nil
}

// ComponentVariable = IndexedVariable | FieldDesignator .
type componentVariable struct {
	*indexedVariable
	*fieldDesignator
	typ
	*field
}

func (p *parser) componentVariable(s *scope, v *variable) *componentVariable {
	switch p.c().ch {
	case '[':
		r := &componentVariable{indexedVariable: p.indexedVariable(s, v)}
		r.typ = r.indexedVariable.typ
		return r
	case '.':
		r := &componentVariable{fieldDesignator: p.fieldDesignator(v)}
		r.typ = r.fieldDesignator.typ
		r.field = r.fieldDesignator.field
		return r
	}

	p.err(p.c(), "unexpected %q, expected indexed variable or field designator", p.c().src)
	p.shift()
	return nil
}

// FieldDesignator
type fieldDesignator struct {
	*variable
	dot   *tok
	ident *tok
	typ
	*field
}

func (p *parser) fieldDesignator(v *variable) *fieldDesignator {
	r := &fieldDesignator{
		variable: v,
		dot:      p.mustShift('.'),
		ident:    p.mustShift(IDENTIFIER),
	}
	fieldType := typ(aInteger)
	if x, ok := v.typ.(*record); ok {
		fld, ok := x.fields[goIdent(r.ident.src)]
		if !ok {
			p.err(r.dot, "unknown field: %s", r.ident.src)
			return nil
		}

		fieldType = fld.typ
		r.field = fld
	} else {
		p.err(p.c(), "selector requires record type: %s", r.typ)
	}
	r.typ = fieldType
	return r
}

// IndexedVariahle = ArrayVariable "[" Index { "," Index } "]" .
type indexedVariable struct {
	*variable
	lbrace *tok
	list   []*expression
	rbrace *tok
	typ
}

func (p *parser) indexedVariable(s *scope, v *variable) *indexedVariable {
	elem := typ(aInteger)
	if x, ok := v.typ.(*array); ok {
		elem = x.elem
	} else {
		p.err(p.c(), "indexing requires array type: %s", v.typ)
	}
	return &indexedVariable{
		v,
		p.mustShift('['),
		p.expressionList(s, nil, false),
		p.mustShift(']'),
		elem,
	}
}

// ProcedureAndFunctionDeclarationPart = { ( ProcedureDeclaration | FunctionDeclaration ) ";" } .
type procedureAndFunctionDeclarationPart struct {
	*procedureDeclaration
	*functionDeclaration
}

func (p *parser) procedureAndFunctionDeclarationPart(s *scope) (r []*procedureAndFunctionDeclarationPart) {
	for {
		switch p.c().ch {
		case PROCEDURE:
			r = append(r, &procedureAndFunctionDeclarationPart{procedureDeclaration: p.procedureDeclaration(s)})
		case FUNCTION:
			r = append(r, &procedureAndFunctionDeclarationPart{functionDeclaration: p.functionDeclaration(s)})
		default:
			return r
		}
		p.mustShift(';')
	}
}

// FunctionDeclaration = FunctionHeading ";" Block |
//	FunctionHeading ";" Directive |
//	FunctionIdentification ";" Block .
type functionDeclaration struct {
	*scope
	*functionHeading
	semi *tok
	*block
	forward *tok
	typ
}

func (n *functionDeclaration) Position() token.Position { return n.functionHeading.function.Position() }

func (n *functionDeclaration) isCompatible(m *functionDeclaration) bool {
	panic(todo("internal error: not implemented"))
}

func (p *parser) functionDeclaration(s *scope) *functionDeclaration {
	r := &functionDeclaration{scope: &scope{parent: s}}
	r.functionHeading = p.functionHeading(r.scope)
	r.typ = r.functionHeading.typ
	s.declare(p, r, r.functionHeading.ident.src)
	r.semi = p.mustShift(';')
	if t := p.c(); t.ch == IDENTIFIER && strings.ToLower(t.src) == "forward" {
		r.forward = p.shift()
		return r
	}

	r.block = p.block(r.scope)
	return r
}

// FunctionHeading = "function" Identifier [ FormalParameterList ] ":" Type .
type functionHeading struct {
	function *tok
	ident    *tok
	list     []*formalParameterSection
	colon    *tok
	*typeNode
	typ
	args []typ
}

func (p *parser) functionHeading(s *scope) *functionHeading {
	r := &functionHeading{
		function: p.mustShift(FUNCTION),
		ident:    p.mustShift(IDENTIFIER),
	}
	r.list, r.args = p.formalParameterList(s)
	r.colon = p.mustShift(':')
	r.typeNode = p.typeNode(s, "")
	if r.typ = r.typeNode.typ; r.typ == nil {
		p.err(r.function, "function type not resolved")
	}
	return r
}

// ProcedureDeclaration = ProcedureHeading ";" Block |
//	ProcedureHeading ";" Directive |
//	ProcedureIdentification ";" Block.
type procedureDeclaration struct {
	*scope
	*procedureHeading
	semi *tok
	*block
	forward *tok

	isRead           bool
	isWrite          bool
	isResetOrRewrite bool
}

func (n *procedureDeclaration) Position() token.Position {
	return n.procedureHeading.procedure.Position()
}

func (n *procedureDeclaration) isCompatible(m *procedureDeclaration) bool {
	return n.procedureHeading.isCompatible(m.procedureHeading)
}

func (p *parser) procedureDeclaration(s *scope) *procedureDeclaration {
	r := &procedureDeclaration{scope: &scope{parent: s}}
	r.procedureHeading = p.procedureHeading(r.scope)
	r.semi = p.mustShift(';')
	s.declare(p, r, r.procedureHeading.ident.src)
	if t := p.c(); t.ch == IDENTIFIER && strings.ToLower(t.src) == "forward" {
		r.forward = p.shift()
		return r
	}

	r.block = p.block(r.scope)
	return r
}

// ProcedureHeading = "procedure" Identifier [ FormalParameterList ] .
type procedureHeading struct {
	procedure *tok
	ident     *tok
	list      []*formalParameterSection
	args      []typ
}

func (p *parser) procedureHeading(s *scope) *procedureHeading {
	r := &procedureHeading{
		procedure: p.mustShift(PROCEDURE),
		ident:     p.mustShift(IDENTIFIER),
	}
	r.list, r.args = p.formalParameterList(s)
	return r
}

func (n *procedureHeading) isCompatible(m *procedureHeading) bool {
	if len(n.list) != len(m.list) {
		return false
	}

	for i, v := range n.list {
		if !v.isCompatible(m.list[i]) {
			return false
		}
	}
	return true
}

func (p *parser) formalParameterList(s *scope) (r []*formalParameterSection, args []typ) {
	if p.c().ch != '(' {
		return nil, nil
	}

	p.shift()
	item := p.formalParameterSection(s)
	r = []*formalParameterSection{item}
	args = append(args, item.args...)
	for p.c().ch == ';' {
		p.shift()
		item = p.formalParameterSection(s)
		r = append(r, item)
		args = append(args, item.args...)
	}
	p.mustShift(')')
	return r, args
}

// FormalParameterSection = ValueParameterSpecification |
//	VariableParamererSpecification |
//	ProceduralParameterSpecification |
//	FunctionalParameterSpecification .
type formalParameterSection struct {
	*valueParameterSpecification
	*variableParameterSpecification
	typ
	args []typ
}

func (n *formalParameterSection) isCompatible(m *formalParameterSection) bool {
	panic(todo("internal error: not implemented"))
}

func (p *parser) formalParameterSection(s *scope) (r *formalParameterSection) {
	defer func() {}()
	switch p.c().ch {
	case VAR:
		r = &formalParameterSection{variableParameterSpecification: p.variableParameterSpecification(s)}
		if r.typ = r.variableParameterSpecification.typ; r.typ == nil {
			p.err(p.c(), "type of formal paramater section not determined")
		}
		for range r.variableParameterSpecification.list {
			r.args = append(r.args, r.typ)
		}
		return r
	case PROCEDURE:
		p.err(p.c(), "procedure parameters not supported")
	case FUNCTION:
		p.err(p.c(), "functional parameters not supported")
	case IDENTIFIER:
		r = &formalParameterSection{valueParameterSpecification: p.valueParameterSpecification(s)}
		if r.typ = r.valueParameterSpecification.typ; r.typ == nil {
			p.err(p.c(), "type of formal paramater section not determined")
		}
		for range r.valueParameterSpecification.list {
			r.args = append(r.args, r.typ)
		}
		return r
	default:
		p.err(p.c(), "unexpected %q, expected value/variable/procedural/functional parameter specification", p.c().src)
	}

	p.shift()
	return nil
}

// VariableParameterSpecification = "var" IdentifierList ":" Type .
type variableParameterSpecification struct {
	var_  *tok
	list  []*tok
	colon *tok
	*typeNode
	typ
}

func (p *parser) variableParameterSpecification(s *scope) *variableParameterSpecification {
	r := &variableParameterSpecification{
		var_:     p.mustShift(VAR),
		list:     p.identifierList(),
		colon:    p.mustShift(':'),
		typeNode: p.typeNode(s, ""),
	}
	for _, v := range r.list {
		s.declare(p, r, v.src)
	}
	r.typ = r.typeNode.typ
	return r
}

// ValueParameterSpecification = IdentifierList ":" Type .
type valueParameterSpecification struct {
	list  []*tok
	colon *tok
	*typeNode
	typ
}

func (p *parser) valueParameterSpecification(s *scope) *valueParameterSpecification {
	r := &valueParameterSpecification{
		list:     p.identifierList(),
		colon:    p.mustShift(':'),
		typeNode: p.typeNode(s, ""),
	}
	for _, v := range r.list {
		s.declare(p, r, v.src)
	}
	r.typ = r.typeNode.typ
	return r
}

// VariableDeclarationPart = [ "var" VariableDeclaration ";" { VariableDeclaration ";" } ] .
type variableDeclarationPart struct {
	var_ *tok
	list []*variableDeclaration
}

func (p *parser) variableDeclarationPart(s *scope) *variableDeclarationPart {
	if p.c().ch != VAR {
		return nil
	}

	return &variableDeclarationPart{
		p.shift(),
		p.variableDeclarationList(s),
	}
}

func (p *parser) variableDeclarationList(s *scope) (r []*variableDeclaration) {
	r = append(r, p.variableDeclaration(s))
	p.mustShift(';')
	for p.c().ch == IDENTIFIER {
		r = append(r, p.variableDeclaration(s))
		p.mustShift(';')
	}
	return r
}

// VariableDeclaration = IdentifierList ":" Type .
type variableDeclaration struct {
	list  []*tok
	colon *tok
	*typeNode
	typ
	used map[string]struct{}
	*scope
}

func (n *variableDeclaration) Position() token.Position { return n.list[0].Position() }

func (p *parser) variableDeclaration(s *scope) *variableDeclaration {
	r := &variableDeclaration{
		list:     p.identifierList(),
		colon:    p.mustShift(':'),
		typeNode: p.typeNode(s, ""),
		used:     map[string]struct{}{},
		scope:    s,
	}
	if r.typ = r.typeNode.typ; r.typ == nil {
		p.err(r.list[0], "variable type not resolved: %s", r.list[0].src)
	}
	for _, v := range r.list {
		s.declare(p, r, v.src)
	}
	return r
}

// TypeDefinitionPart = [ "type" TypeDefinition ";" { TypeDefinitionPart ";" } ] .
type typeDefinitionPart struct {
	typ  *tok
	list []*typeDefinition
}

func (p *parser) typeDefinitionPart(s *scope) *typeDefinitionPart {
	if p.c().ch != TYPE {
		return nil
	}

	return &typeDefinitionPart{
		p.shift(),
		p.typeDefinitionList(s),
	}
}

// TypeDefinition = Identifier "=" Type .
type typeDefinition struct {
	ident *tok
	eq    *tok
	*typeNode
	typ
}

func (n *typeDefinition) Position() token.Position { return n.ident.Position() }

func (p *parser) typeDefinition(s *scope) *typeDefinition {
	if p.c().ch != IDENTIFIER {
		return nil
	}

	tag := p.c().src
	r := &typeDefinition{
		ident:    p.mustShift(IDENTIFIER),
		eq:       p.mustShift('='),
		typeNode: p.typeNode(s, tag),
	}
	s.declare(p, r, r.ident.src)
	if r.typ = r.typeNode.typ; r.typ == nil {
		p.err(r.ident, "type not resolved: %s", r.ident.src)
	}
	return r
}

// Type = SimpleType | StructuredType | PointerType .
type typeNode struct {
	*simpleType
	*structuredType
	typ
}

func (p *parser) typeNode(s *scope, tag string) *typeNode {
	switch p.c().ch {
	case INT_LITERAL, IDENTIFIER, '-':
		r := &typeNode{simpleType: p.simpleType(s)}
		r.typ = r.simpleType.typ
		return r
	case PACKED, RECORD, FILE, ARRAY:
		r := &typeNode{structuredType: p.structuredType(s, tag)}
		r.typ = r.structuredType.typ
		return r
	}

	p.err(p.c(), "unexpected %q, expected simple type or structured type or pointer type", p.c().src)
	p.shift()
	return nil
}

// StructuredType = [ "packed" ] UnpackedStructuredType | StructuredTypeIdentifier .
type structuredType struct {
	packed *tok
	*unpackedStructuredType
	typ
}

func (p *parser) structuredType(s *scope, tag string) *structuredType {
	if p.c().ch == PACKED {
		r := &structuredType{
			packed:                 p.shift(),
			unpackedStructuredType: p.unpackedStructuredType(s, tag),
		}
		r.typ = r.unpackedStructuredType.typ
		if r.typ != nil {
			r.typ.(packer).setPacked()
		}
		return r
	}

	r := &structuredType{
		unpackedStructuredType: p.unpackedStructuredType(s, tag),
	}
	r.typ = r.unpackedStructuredType.typ
	return r
}

// UnpackedStructuredType = ArrayType | RecordType | SetType | FileType .
type unpackedStructuredType struct {
	*arrayType
	*fileType
	*recordType
	typ
}

func (p *parser) unpackedStructuredType(s *scope, tag string) *unpackedStructuredType {
	switch p.c().ch {
	case FILE:
		r := &unpackedStructuredType{fileType: p.fileType(s)}
		r.typ = r.fileType.typ
		return r
	case RECORD:
		r := &unpackedStructuredType{recordType: p.recordType(s, tag)}
		r.typ = r.recordType.typ
		return r
	case ARRAY:
		r := &unpackedStructuredType{arrayType: p.arrayType(s)}
		r.typ = r.arrayType.typ
		return r
	}

	p.err(p.c(), "unexpected %q, expected arary type or record type or set type or file type", p.c().src)
	p.shift()
	return nil
}

// ArrayType = "array" "[" IndexType { "," IndexType } "]" "of" ComponentType .
type arrayType struct {
	array  *tok
	lbrace *tok
	list   []*typeNode
	rbrace *tok
	of     *tok
	*typeNode
	typ
}

func (p *parser) arrayType(s *scope) *arrayType {
	r := &arrayType{
		array:    p.mustShift(ARRAY),
		lbrace:   p.mustShift('['),
		list:     p.typeList(s),
		rbrace:   p.mustShift(']'),
		of:       p.mustShift(OF),
		typeNode: p.typeNode(s, ""),
	}
	for _, v := range r.list {
		if v.typ == nil {
			p.err(r, "array dimension type not resolved")
			return r
		}
	}

	if r.typeNode.typ == nil {
		p.err(r, "array element type not resolved")
		return r
	}

	var err error
	if r.typ, err = newArray(r.array, r.list, r.typeNode.typ); err != nil {
		p.err(r.array, "%s", err)
	}
	return r
}

func (p *parser) typeList(s *scope) (r []*typeNode) {
	r = append(r, p.typeNode(s, ""))
	for p.c().ch == ',' {
		r = append(r, p.typeNode(s, ""))
	}
	return r
}

// RecordType = "record" FieldList "end" .
type recordType struct {
	record *tok
	*fieldList
	end *tok
	typ
}

func (p *parser) recordType(s *scope, tag string) *recordType {
	r := &recordType{
		record:    p.mustShift(RECORD),
		fieldList: p.fieldList(s),
		end:       p.mustShift(END),
	}
	var err error
	if r.typ, err = newRecord(runtime.GOOS, runtime.GOARCH, r.fieldList, tag); err != nil {
		p.err(r.record, "compute layout: %v", err)
	}
	return r
}

// FieldList = [ ( FixedPart [ ";" VariantPart ] | VariantPart ) [ ";" ] ] .
type fieldList struct {
	fixedPart []*recordSection
	*variantPart
}

func (n *fieldList) collect(m map[string]*field) (list []*field, err error) {
	for _, v := range n.fixedPart {
		for _, w := range v.list {
			nm := goIdent(w.src)
			if _, ok := m[nm]; ok {
				return nil, fmt.Errorf("duplicate field: %s", nm)
			}

			fld := &field{name: nm, typ: v.typ}
			m[nm] = fld
			list = append(list, fld)
		}
	}
	return list, nil
}

func (n *fieldList) structLiteral(w strutil.Formatter) error {
	w.Format("\nstruct {%i")
	defer w.Format("%u\n}")
	for _, v := range n.fixedPart {
		w.Format("\n")
		if err := v.structLiteral(w); err != nil {
			return err
		}
	}
	return n.variantPart.structLiteral(w)
}

func (n *fieldList) c(w strutil.Formatter, mustWrapInStruct bool, tag string) bool {
	if len(n.fixedPart) > 1 || n.variantPart != nil {
		mustWrapInStruct = true
	}
	if mustWrapInStruct {
		switch {
		case tag == "":
			w.Format("\nstruct {%i")
		default:
			w.Format("\nstruct %s {%i", tag)
		}
		defer w.Format("%u\n}")
	}
	for _, v := range n.fixedPart {
		w.Format("\n")
		v.c(w)
		w.Format(";")
	}
	if n.variantPart != nil {
		w.Format("\n")
		n.variantPart.c(w)
		w.Format(";")
	}
	return mustWrapInStruct
}

func (p *parser) fieldList(s *scope) *fieldList {
	switch p.c().ch {
	case IDENTIFIER:
		r := &fieldList{fixedPart: p.fixedPart(s)}
		switch p.c().ch {
		case ';':
			p.shift()
			switch p.c().ch {
			case CASE:
				r.variantPart = p.variantPart(s)
				if p.c().ch == ';' {
					p.shift()
				}
				return r
			case END:
				return r
			default:
				p.err(p.c(), "unexpected %q, expected 'case' or 'end'", p.c().src)
				p.shift()
				return nil
			}
		case ')':
			return r
		default:
			p.err(p.c(), "unexpected %q, expected ';' or ','", p.c().src)
			p.shift()
			return nil
		}
	case CASE:
		r := &fieldList{variantPart: p.variantPart(s)}
		if p.c().ch == ';' {
			p.shift()
		}
		return r
	}

	p.err(p.c(), "unexpected %q, expected fixed part or variant part", p.c().src)
	p.shift()
	return nil
}

// VariantPart = "case" VariantSelector "of" Variant { ";" Variant } .
type variantPart struct {
	case_ *tok
	*variantSelector
	of    *tok
	list  []*variant
	align uintptr
	size  uintptr
}

func (n *variantPart) structLiteral(w strutil.Formatter) error {
	if n == nil {
		return nil
	}

	// Tag fields not used so far.
	// w.Format("\n%s\t%s", n.variantSelector.tagType.src, n.variantSelector.typ.goType())
	if n.align != n.size {
		return fmt.Errorf("variant parts requiring padding not supported: align %v, size %v", n.align, n.size)
	}

	w.Format("\nvariant\t")
	switch n.align {
	case 1:
		w.Format("byte")
	case 2:
		w.Format("uint16")
	case 4:
		w.Format("uint32")
	case 8:
		w.Format("float64")
	default:
		return fmt.Errorf("unsupported variant part alignment: %v", n.align)
	}
	return nil
}

func (n *variantPart) collect(m map[string]*field) (list []*field, err error) {
	n.variantSelector.collect(m)
	for _, v := range n.list {
		part, err := v.fieldList.collect(m)
		if err != nil {
			return nil, err
		}

		list = append(list, part...)
	}
	return list, nil
}

func (n *variantPart) c(w strutil.Formatter) {
	if n == nil {
		return
	}

	// Tag fields not used so far.
	// w.Format("%s\t%s;", n.variantSelector.typ.cType(), strings.ToUpper(n.variantSelector.tagType.src))
	w.Format("\nunion {%i")
	for _, v := range n.list {
		if v.fieldList.c(w, false, "") {
			w.Format(";")
		}
	}
	w.Format("%u\n} %s", idU)

}

func (p *parser) variantPart(s *scope) *variantPart {
	return &variantPart{
		case_:           p.mustShift(CASE),
		variantSelector: p.variantSelector(s),
		of:              p.mustShift(OF),
		list:            p.variantList(s),
	}
}

// Variant { ";" Variant }
func (p *parser) variantList(s *scope) (r []*variant) {
	v := p.variant(s)
	if v == nil {
		return nil
	}

	r = []*variant{v}
	for p.c().ch == ';' {
		semi := p.shift()
		if p.c().ch == END {
			p.unget(semi)
			break
		}

		r = append(r, p.variant(s))
	}
	return r
}

// Variant = Constant { "," Constant } ":" "(" FieIdList ")" .
type variant struct {
	consts []*constant
	colon  *tok
	lparen *tok
	*fieldList
	rparen *tok
}

func (p *parser) variant(s *scope) *variant {
	c := p.constant(s)
	if c == nil {
		return nil
	}

	r := &variant{consts: []*constant{c}}
	for p.c().ch == ',' {
		p.shift()
		r.consts = append(r.consts, p.constant(s))
	}
	r.colon = p.mustShift(':')
	r.lparen = p.mustShift('(')
	r.fieldList = p.fieldList(s)
	r.rparen = p.mustShift(')')
	return r
}

// VariantSelector = [ TagField ":"] TagType .
type variantSelector struct {
	tagField *tok
	colon    *tok
	tagType  *tok
	typ
}

func (n *variantSelector) collect(m map[string]*field) error {
	nm := n.tagType.src
	if _, ok := m[nm]; ok {
		return fmt.Errorf("duplicated field: %s", nm)
	}

	m[nm] = &field{typ: n.typ}
	return nil
}

func (p *parser) variantSelector(s *scope) *variantSelector {
	tok := p.mustShift(IDENTIFIER)
	switch p.c().ch {
	case ':':
		p.err(tok, "tag fields not supported")
	default:
		r := &variantSelector{tagType: tok}
		_, n := s.find(r.tagType.src)
		switch x := n.(type) {
		case nil:
			p.err(r.tagType, "undefined: %s", r.tagType.src)
		case *typeDefinition:
			r.typ = x.typ
			if _, ok := r.typ.(ordinal); !ok {
				p.err(tok, "ordinal type required: %s", tok.src)
			}
		default:
			p.err(r.tagType, "not a type: %s", r.tagType.src)
		}
		return r
	}

	p.shift()
	return nil
}

// FixedPart = RecordSection { ";" RecordSection } .
func (p *parser) fixedPart(s *scope) []*recordSection {
	rs := p.recordSection(s)
	if rs == nil {
		return nil
	}

	r := []*recordSection{rs}
	for p.c().ch == ';' {
		semi := p.shift()
		if ch := p.c().ch; ch == CASE || ch == END {
			p.unget(semi)
			break
		}

		if rs = p.recordSection(s); rs == nil {
			break
		}

		r = append(r, rs)
	}
	return r
}

// RecordSection = IdentifierList ":" Type.
type recordSection struct {
	list  []*tok
	colon *tok
	*typeNode
	typ
}

func (n *recordSection) structLiteral(w strutil.Formatter) error {
	for _, v := range n.list {
		w.Format("\n%s\t%s", goIdent(v.src), n.typ.goType())
	}
	return nil
}

func (n *recordSection) c(w strutil.Formatter) {
	a := strings.Split(n.typ.cType(), "\n")
	for i, v := range a {
		w.Format("%s", v)
		if i != len(a)-1 {
			w.Format("\n")
		}
	}
	w.Format("\t")
	for i, v := range n.list {
		w.Format("%s", strings.ToUpper(v.src))
		if i != len(n.list)-1 {
			w.Format(", ")
		}
	}
}

func (p *parser) recordSection(s *scope) *recordSection {
	r := &recordSection{
		list:     p.identifierList(),
		colon:    p.mustShift(':'),
		typeNode: p.typeNode(s, ""),
	}
	r.typ = r.typeNode.typ
	return r
}

// IdentifierList = Identifier { "," Identifier } .
func (p *parser) identifierList() []*tok {
	t := p.mustShift(IDENTIFIER)
	if t == nil {
		return nil
	}

	r := []*tok{t}
	for p.c().ch == ',' {
		p.shift()
		if t = p.mustShift(IDENTIFIER); t == nil {
			break
		}

		r = append(r, t)
	}
	return r
}

// FileType = "file" "of" Type .
type fileType struct {
	file *tok
	of   *tok
	*typeNode
	typ
}

func (p *parser) fileType(s *scope) *fileType {
	r := &fileType{
		file:     p.mustShift(FILE),
		of:       p.mustShift(OF),
		typeNode: p.typeNode(s, ""),
	}
	r.typ = newFile(r.typeNode.typ)
	return r
}

// SimpleType = OrdinalType | RealTypeldentifier.
type simpleType struct {
	*ordinalType
	*identifier
	typ
}

func (p *parser) simpleType(s *scope) *simpleType {
	switch p.c().ch {
	case INT_LITERAL, '-':
		r := &simpleType{ordinalType: p.ordinalType(s)}
		r.typ = r.ordinalType.typ
		return r
	case IDENTIFIER:
		id := p.shift()
		if p.c().ch != DD {
			p.unget(id)
			r := &simpleType{identifier: p.identifier(s, false)}
			switch x := r.identifier.def.(type) {
			case nil:
				// already reported
			case *typeDefinition:
				r.typ = x.typ
			case typ:
				r.typ = x
			default:
				p.err(r.identifier.tok, "not a type: %s", r.identifier.src)
			}
			return r
		}

		p.unget(id)
		r := &simpleType{ordinalType: p.ordinalType(s)}
		r.typ = r.ordinalType.typ
		return r
	}

	p.err(p.c(), "unexpected %q, expected ordinal type or real type identifier", p.c().src)
	p.shift()
	return nil
}

// OrdinalType = EnumeratedType | SubrangeType | OrdinalTypeldentifier .
type ordinalType struct {
	*subrangeType
	ident *tok
	typ
}

func (p *parser) ordinalType(s *scope) *ordinalType {
	switch p.c().ch {
	case INT_LITERAL, '-':
		r := &ordinalType{subrangeType: p.subrangeType(s)}
		r.typ = r.subrangeType.typ
		return r
	case IDENTIFIER:
		id := p.shift()
		if p.c().ch != DD {
			return &ordinalType{ident: id}
		}

		p.unget(id)
		r := &ordinalType{subrangeType: p.subrangeType(s)}
		r.typ = r.subrangeType.typ
		return r
	}

	p.err(p.c(), "unexpected %q, expected enumerated type or subrange type or ordinal type identifier", p.c().src)
	p.shift()
	return nil
}

// SubrangeType = Constant ".." Constant .
type subrangeType struct {
	first *constant
	dd    *tok
	last  *constant
	typ
}

func (p *parser) subrangeType(s *scope) *subrangeType {
	r := &subrangeType{
		first: p.constant(s),
		dd:    p.mustShift(DD),
		last:  p.constant(s),
	}
	var first, last int
	switch x := r.first.literal.(type) {
	case integerLiteral:
		first = int(x)
	default:
		p.err(r.first, "expected integer constant")
	}
	switch x := r.last.literal.(type) {
	case integerLiteral:
		last = int(x)
	default:
		p.err(r.first, "expected integer constant")
	}
	var err error
	if r.typ, err = newSubrange(first, last); err != nil {
		p.err(r.dd, "%s", err)
	}
	return r
}

// TypeDefinition ";" { TypeDefinitionPart ";" }
func (p *parser) typeDefinitionList(s *scope) (r []*typeDefinition) {
	td := p.typeDefinition(s)
	if td == nil {
		return nil
	}

	p.mustShift(';')
	r = []*typeDefinition{td}
	for {
		td = p.typeDefinition(s)
		if td == nil {
			return r
		}

		p.mustShift(';')
		r = append(r, td)
	}
}

// ConstantDefinitionPart = [ "const" ConstantDefinition ";" { ConstantDefinition ";" } ] .
type constantDefinitionPart struct {
	const_ *tok
	list   []*constantDefinition
}

func (p *parser) constantDefinitionPart(s *scope) *constantDefinitionPart {
	if p.c().ch != CONST {
		return nil
	}

	return &constantDefinitionPart{
		p.shift(),
		p.constantDefinitionList(s),
	}
}

// ConstantDefinition = Identifier "=" Constant .
type constantDefinition struct {
	ident *tok
	eq    *tok
	*constant
}

func (n *constantDefinition) Position() token.Position { return n.ident.Position() }

func (p *parser) constantDefinition(s *scope) *constantDefinition {
	if p.c().ch != IDENTIFIER {
		return nil
	}

	r := &constantDefinition{
		ident:    p.shift(),
		eq:       p.mustShift('='),
		constant: p.constant(s),
	}
	s.declare(p, r, r.ident.src)
	if r.literal == nil || r.literal.typ() == nil {
		p.err(r.ident, "constant value/type undetermined: %s", r.ident.src)
	}
	return r
}

// Constant = [ Sign ] ( UnsignedNumher | ConstantIdentifier) | CharacterString .
type constant struct {
	noder
	sign  *tok
	num   *tok
	ident *tok
	str   *tok
	literal
}

func (n *constant) resolveSymbol(p *parser, s *scope) {
	// const foo = bar;
	nm := n.ident.src
	_, nd := s.find(nm)
	switch x := nd.(type) {
	case nil:
		p.err(n.ident, "undefined: %s", nm)
	case *constantDefinition:
		n.literal = x.constant.literal
	default:
		p.err(n.ident, "not a constant: %s (%T)", nm, x)
	}
}

func (p *parser) constant(s *scope) (r *constant) {
	c0 := p.c()
	var neg bool
	var err error

	defer func() {
		if r != nil {
			if r.literal == nil || r.literal.typ() == nil {
				p.err(c0, "type or value not resolved")
			}

			if neg {
				r.literal = p.inverseLiteral(c0, r.literal)
			}
		}
	}()

	sign := p.sign()
	neg = sign != nil && sign.src == "-"
	if sign = p.sign(); sign != nil {
		sign = p.shift()
		switch p.c().ch {
		case INT_LITERAL:
			r := &constant{
				sign: sign,
				num:  p.shift(),
			}
			if r.literal, err = newIntegerLiteralFromString(r.num.src); err != nil {
				p.err(r.num, "%v", err)
			}
			return r
		case IDENTIFIER:
			r = &constant{
				sign:  sign,
				ident: p.shift(),
			}
			r.resolveSymbol(p, s)
			return r
		default:
			p.err(p.c(), "expected unsigned number or identifier")
			p.shift()
			return nil
		}
	}

	switch p.c().ch {
	case INT_LITERAL:
		r := &constant{
			num: p.shift(),
		}
		if r.literal, err = newIntegerLiteralFromString(r.num.src); err != nil {
			p.err(r.num, "%v", err)
		}
		return r
	case IDENTIFIER:
		r = &constant{
			ident: p.shift(),
		}
		r.resolveSymbol(p, s)
		return r
	case STR_LITERAL:
		str := p.shift()
		switch s := goStringFromPascalString(str.src); len(s) {
		case 0:
			p.err(str, "empty string constants not supported")
		case 1:
			p.err(str, "char constants not supported")
		default:
			r := &constant{
				str: str,
			}
			var err error
			if r.literal, err = newLiteralFromString(goStringFromPascalString(str.src)); err != nil {
				p.err(c0, "%s", err)
			}
			return r
		}
	}

	p.err(p.c(), "expected constant")
	p.shift()
	return nil
}

func (p *parser) inverseLiteral(n node, l literal) literal {
	if l == nil || l.typ() == nil {
		return l
	}

	x, ok := l.(literalInvertor)
	if !ok {
		p.err(n, "negation of a wrong operand type: %v", l.typ())
		return l
	}

	return x.inverse()
}

func goStringFromPascalString(s string) string {
	s = s[1 : len(s)-1]
	return strings.ReplaceAll(s, "''", "'")
}

func (p *parser) sign() *tok {
	switch p.c().ch {
	case '+', '-':
		return p.shift()
	}

	return nil
}

// ConstantDefinition ";" { ConstantDefinition ";" }
func (p *parser) constantDefinitionList(s *scope) (r []*constantDefinition) {
	cd := p.constantDefinition(s)
	if cd == nil {
		return nil
	}

	p.mustShift(';')
	r = []*constantDefinition{cd}
	for {
		cd = p.constantDefinition(s)
		if cd == nil {
			return r
		}

		p.mustShift(';')
		r = append(r, cd)
	}
}

// LabelDeclarationPart = [ "label" DigitSequence { "," DigitSequence } ";" ]
type labelDeclarationPart struct {
	label *tok
	list  []*tok
	semi  *tok
}

func (n *labelDeclarationPart) Position() token.Position { return n.label.Position() }

func (p *parser) labelDeclarationPart(s *scope) *labelDeclarationPart {
	if p.c().ch != LABEL {
		return nil
	}

	r := &labelDeclarationPart{
		p.shift(),
		p.labelList(),
		p.mustShift(';'),
	}
	for _, v := range r.list {
		s.declare(p, v, v.src)
	}
	return r
}

func (p *parser) labelList() (r []*tok) {
	tok := p.mustShift(INT_LITERAL)
	if tok == nil {
		return r
	}

	r = append(r, tok)
	for p.c().ch == ',' {
		p.shift()
		tok := p.mustShift(INT_LITERAL)
		if tok == nil {
			break
		}

		r = append(r, tok)
	}
	return r
}

// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
	"go/token"
	"strings"
)

func parse(b []byte, name string) (*program, error) {
	p, err := newParser(b, name)
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
	tok      *tok // current token if not nil and ungetBuf is nil
	ungetBuf *tok // current token if not nil
	universe scope

	debug          bool
	overflowCheck  bool
	rangeCheck     bool
	seenDirectives bool
}

func newParser(b []byte, name string) (*parser, error) {
	s, err := newScanner(b, name)
	if err != nil {
		return nil, err
	}

	return &parser{
		scanner: s,
		universe: scope{
			m: map[string]node{
				"boolean": &boolean{},
				"char":    &char{},
				"integer": &integer{},
				"real":    &real{},
				"true": &constantDefinition{
					ident:    &tok{ch: IDENTIFIER, src: "false"},
					constant: &constant{op: newBooleanOperand(false)},
				},
				"false": &constantDefinition{
					ident:    &tok{ch: IDENTIFIER, src: "true"},
					constant: &constant{op: newBooleanOperand(true)},
				},
			},
		},
	}, nil
}

func (p *parser) err(n node, msg string, args ...interface{}) {
	if !p.allErrors && len(p.errs) == 10 {
		return
	}

	pos := fmt.Sprintf("%v: ", n.Position())
	msg = fmt.Sprintf(pos+msg, args...)
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
	scope
	*programHeading
	semi *tok
	*block
	dot *tok
}

func (p *parser) program() *program {
	var r program
	r.scope.parent = &p.universe
	r.programHeading = p.programHeading()
	r.semi = p.mustShift(';')
	r.block = p.block(&r.scope)
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
		p.compoundStatement(s),
	}
}

// CompoundStatement = "begin" StatementSequence "end" .
type compoundStatement struct {
	begin *tok
	list  []*statement
	end   *tok
}

func (p *parser) compoundStatement(s *scope) *compoundStatement {
	return &compoundStatement{
		p.mustShift(BEGIN),
		p.statementList(s),
		p.mustShift(END),
	}
}

// StatementSequence = Statement { ";" Statement} .
func (p *parser) statementList(s *scope) (r []*statement) {
	r = []*statement{p.statement(s)}
	for p.c().ch == ';' {
		p.shift()
		switch p.c().ch {
		case UNTIL, ELSE, END:
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
}

func (p *parser) statement(s *scope) *statement {
	var label *tok
	if p.c().ch == INT_LITERAL {
		label = p.shift()
		p.mustShift(':')
	}
	switch p.c().ch {
	case BEGIN, IF, WHILE, REPEAT, FOR, WITH, CASE:
		return &statement{
			label:                label,
			structuredStatmenent: p.structuredStatmenent(s),
		}
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
}

func (p *parser) structuredStatmenent(s *scope) *structuredStatmenent {
	switch p.c().ch {
	case FOR, WHILE, REPEAT:
		return &structuredStatmenent{repetitiveStatement: p.repetitiveStatement(s)}
	case BEGIN:
		return &structuredStatmenent{compoundStatement: p.compoundStatement(s)}
	case CASE, IF:
		return &structuredStatmenent{conditionalStatement: p.conditionalStatement(s)}
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

// CaseStatement = "case" Caselndex "of"
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
	list    []*constant
	colon   *tok
	elseTok *tok
	*statement
}

func (p *parser) case_(s *scope) *case_ {
	if p.c().ch == ELSE {
		return &case_{
			elseTok:   p.shift(),
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
		p.identifier(s),
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
	s *scope
	n node
}

func (p *parser) identifier(s *scope) *identifier {
	r := &identifier{tok: p.mustShift(IDENTIFIER)}
	if r.tok != nil {
		if r.s, r.n = s.find(r.tok.src); r.n == nil {
			p.err(r.tok, "undefined: %s", r.tok.src)
		}
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
}

func (p *parser) simpleStatement(s *scope) *simpleStatement {
	switch p.c().ch {
	case ';', END:
		return &simpleStatement{}
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
	ident *tok
	list  []*arg
}

func (p *parser) procedureStatement(s *scope) *procedureStatement {
	return &procedureStatement{
		p.mustShift(IDENTIFIER),
		p.argList(s),
	}
}

func (p *parser) argList(s *scope) (r []*arg) {
	if p.c().ch != '(' {
		return nil
	}

	p.shift()
	r = []*arg{p.arg(s)}
	for p.c().ch == ',' {
		p.shift()
		r = append(r, p.arg(s))
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

func (p *parser) arg(s *scope) *arg {
	r := &arg{expression: p.expression(s)}
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

// AssignmentStatement = ( Variable | FunctionIdentifier ) ":=" Expression .
type assignmentStatement struct {
	*variable
	assign *tok
	*expression
}

func (p *parser) assignmentStatement(s *scope) *assignmentStatement {
	return &assignmentStatement{
		p.variable(s),
		p.mustShift(ASSIGN),
		p.expression(s),
	}
}

// Expression = SimpleExression [ RelationalOperator SimpleExression ] .
type expression struct {
	*simpleExpression
	relOp *tok
	rhs   *simpleExpression
}

func (p *parser) expression(s *scope) *expression {
	r := &expression{simpleExpression: p.simpleExpression(s)}
	switch p.c().ch {
	case '=', NE, '<', LE, '>', GE, IN:
		r.relOp = p.shift()
		r.rhs = p.simpleExpression(s)
	}
	return r
}

// Term = Factor { MultiplyingOperator Factor } .
type term struct {
	*factor
	list []termListItem
}

type termListItem struct {
	mulOp *tok
	*factor
}

// a*b*c = (a*b)*c
func (p *parser) term(s *scope) *term {
	r := &term{factor: p.factor(s)}
	for {
		switch p.c().ch {
		case '*', '/', DIV, MOD, AND:
			r.list = append(r.list, termListItem{
				p.shift(),
				p.factor(s),
			})
		default:
			return r
		}
	}
}

// Factor = UnsignedConstant | BoundIdentifier | Variable
//	| SetConstructor | FunctionDesignator |
//	"not" factor | "(" Expression ")" .
type factor struct {
	*unsignedConstant
	ident *tok
	*functionDesignator
	*variable
	*expression
	not *factor
}

func (p *parser) factor(s *scope) *factor {
	switch p.c().ch {
	case INT_LITERAL, STR_LITERAL, REAL_LITERAL:
		return &factor{unsignedConstant: p.unsignedConstant()}
	case IDENTIFIER:
		id := p.shift()
		if p.c().ch == '(' {
			p.unget(id)
			return &factor{functionDesignator: p.functionDesignator(s)}
		}

		p.unget(id)
		return &factor{variable: p.variable(s)}
	case '(':
		p.shift()
		r := &factor{expression: p.expression(s)}
		p.mustShift(')')
		return r
	case NOT:
		p.shift()
		return &factor{not: p.factor(s)}
	}

	p.err(p.c(), "unexpected %q, expected unsigned constant or varible or set constructor or 'not' or '( expression ')'", p.c().src)
	p.shift()
	return nil
}

// FunctionDesignator = FunctionIdentifier [ ActualParameterList ] .
type functionDesignator struct {
	ident *tok
	list  []*expression
}

func (p *parser) functionDesignator(s *scope) *functionDesignator {
	return &functionDesignator{
		p.mustShift(IDENTIFIER),
		p.actualParameterList(s),
	}
}

func (p *parser) actualParameterList(s *scope) (r []*expression) {
	if p.c().ch != '(' {
		return nil
	}

	p.shift()
	r = p.expressionList(s)
	p.mustShift(')')
	return r
}

// UnsignedConstant = UnsignedNumber | CharacterString | ConstantIdentifier | "nil" .
type unsignedConstant struct {
	*unsignedNumber
	str   *tok
	ident *tok
}

func (p *parser) unsignedConstant() *unsignedConstant {
	switch p.c().ch {
	case INT_LITERAL, REAL_LITERAL:
		return &unsignedConstant{unsignedNumber: p.unsignedNumber()}
	case STR_LITERAL:
		return &unsignedConstant{str: p.shift()}
	}

	p.err(p.c(), "unexpected %q, expected unsigned number or character string or identifier or 'nil'", p.c().src)
	p.shift()
	return nil
}

// UnsignedNumber = UnsignedInteger | UnsignedReal .
type unsignedNumber struct {
	int  *tok
	real *tok
}

func (p *parser) unsignedNumber() *unsignedNumber {
	switch p.c().ch {
	case INT_LITERAL:
		return &unsignedNumber{int: p.shift()}
	case REAL_LITERAL:
		return &unsignedNumber{real: p.shift()}
	}

	p.err(p.c(), "unexpected %q, expected unsigned integer or unsigned real", p.c().src)
	p.shift()
	return nil
}

// SimpleExpression = [ Sign ] Term { AddingOperator Term } .
type simpleExpression struct {
	sign *tok
	*term
	list []simpleExpressionListItem
}

type simpleExpressionListItem struct {
	addOp *tok
	*term
}

func (p *parser) simpleExpression(s *scope) *simpleExpression {
	sign := p.sign()
	r := &simpleExpression{
		sign: sign,
		term: p.term(s),
	}
	for {
		switch p.c().ch {
		case '+', '-', OR:
			r.list = append(r.list, simpleExpressionListItem{
				p.shift(),
				p.term(s),
			})
		default:
			return r
		}
	}
}

func (p *parser) expressionList(s *scope) (r []*expression) {
	r = []*expression{p.expression(s)}
	for p.c().ch == ',' {
		p.shift()
		r = append(r, p.expression(s))
	}
	return r
}

// Variable = EntireVariable | ComponentVariable |
//	IdentifiedVariable | BufferVariable .
type variable struct {
	*identifier
	*componentVariable
	deref *variable
}

func (p *parser) variable(s *scope) (r *variable) {
	switch p.c().ch {
	case IDENTIFIER:
		r = &variable{identifier: p.identifier(s)}
		for {
			switch p.c().ch {
			case '[', '.':
				r = &variable{componentVariable: p.componentVariable(s, r)}
			case '^':
				p.shift()
				r = &variable{deref: r}
			default:
				return r
			}
		}
	}

	p.err(p.c(), "unexpected %q, expected variable identifier or component variable or identified variable of buffer variable", p.c().src)
	p.shift()
	return nil
}

// ComponentVariable = IndexedVariahle | FieldDesignator .
type componentVariable struct {
	*indexedVariable
	*fieldDesignator
}

func (p *parser) componentVariable(s *scope, v *variable) *componentVariable {
	switch p.c().ch {
	case '[':
		return &componentVariable{indexedVariable: p.indexedVariable(s, v)}
	case '.':
		return &componentVariable{fieldDesignator: p.fieldDesignator(v)}
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
}

func (p *parser) fieldDesignator(v *variable) *fieldDesignator {
	return &fieldDesignator{
		v,
		p.mustShift('.'),
		p.mustShift(IDENTIFIER), //TODO
	}
}

// IndexedVariahle = ArrayVariable "[" Index { "," Index } "]" .
type indexedVariable struct {
	*variable
	lbrace *tok
	list   []*expression
	rbrace *tok
}

func (p *parser) indexedVariable(s *scope, v *variable) *indexedVariable {
	return &indexedVariable{
		v,
		p.mustShift('['),
		p.expressionList(s),
		p.mustShift(']'),
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
	scope
	*functionHeading
	semi *tok
	*block
	forward *tok
}

func (n *functionDeclaration) Position() token.Position { return n.functionHeading.function.Position() }

func (n *functionDeclaration) isCompatible(m *functionDeclaration) bool {
	panic(todo(""))
}

func (p *parser) functionDeclaration(s *scope) *functionDeclaration {
	r := &functionDeclaration{scope: scope{parent: s}}
	r.functionHeading = p.functionHeading(s)
	for _, v := range r.functionHeading.list {
		for _, w := range v.names() {
			r.scope.declare(p, w, w.src)
		}
	}
	s.declare(p, r, r.functionHeading.ident.src)
	r.semi = p.mustShift(';')
	if t := p.c(); t.ch == IDENTIFIER && strings.ToLower(t.src) == "forward" {
		r.forward = p.shift()
		return r
	}

	r.block = p.block(&r.scope)
	return r
}

// FunctionHeading = "function" Identifier [ FormalParameterList ] ":" Type .
type functionHeading struct {
	function *tok
	ident    *tok
	list     []*formalParameterSection
	colon    *tok
	*typeNode
}

func (p *parser) functionHeading(s *scope) *functionHeading {
	return &functionHeading{
		p.mustShift(FUNCTION),
		p.mustShift(IDENTIFIER),
		p.formalParameterList(s),
		p.mustShift(':'),
		p.typeNode(s),
	}
}

// ProcedureDeclaration = ProcedureHeading ";" Block |
//	ProcedureHeading ";" Directive |
//	ProcedureIdentification ";" Block.
type procedureDeclaration struct {
	scope
	*procedureHeading
	semi *tok
	*block
	forward *tok
}

func (n *procedureDeclaration) Position() token.Position {
	return n.procedureHeading.procedure.Position()
}

func (n *procedureDeclaration) isCompatible(m *procedureDeclaration) bool {
	return n.procedureHeading.isCompatible(m.procedureHeading)
}

func (p *parser) procedureDeclaration(s *scope) *procedureDeclaration {
	r := &procedureDeclaration{scope: scope{parent: s}}
	r.procedureHeading = p.procedureHeading(s)
	for _, v := range r.procedureHeading.list {
		for _, w := range v.names() {
			r.scope.declare(p, w, w.src)
		}
	}
	s.declare(p, r, r.procedureHeading.ident.src)
	r.semi = p.mustShift(';')
	if t := p.c(); t.ch == IDENTIFIER && strings.ToLower(t.src) == "forward" {
		r.forward = p.shift()
		return r
	}

	r.block = p.block(&r.scope)
	return r
}

// ProcedureHeading = "procedure" Identifier [ FormalParameterList ] .
type procedureHeading struct {
	procedure *tok
	ident     *tok
	list      []*formalParameterSection
}

func (p *parser) procedureHeading(s *scope) *procedureHeading {
	return &procedureHeading{
		p.mustShift(PROCEDURE),
		p.mustShift(IDENTIFIER),
		p.formalParameterList(s),
	}
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

func (p *parser) formalParameterList(s *scope) (r []*formalParameterSection) {
	if p.c().ch != '(' {
		return nil
	}

	p.shift()
	r = []*formalParameterSection{p.formalParameterSection(s)}
	for p.c().ch == ';' {
		p.shift()
		r = append(r, p.formalParameterSection(s))
	}
	p.mustShift(')')
	return r
}

// FormalParameterSection = ValueParameterSpecification |
//	VariableParamererSpecification |
//	ProceduralParameterSpecification |
//	FunctionalParameterSpecification .
type formalParameterSection struct {
	*valueParameterSpecification
	*variableParameterSpecification
}

func (n *formalParameterSection) names() []*tok {
	if n.valueParameterSpecification != nil {
		return n.valueParameterSpecification.list
	}

	return n.variableParameterSpecification.list
}

func (n *formalParameterSection) isCompatible(m *formalParameterSection) bool {
	panic(todo(""))
}

func (p *parser) formalParameterSection(s *scope) *formalParameterSection {
	switch p.c().ch {
	case VAR:
		return &formalParameterSection{variableParameterSpecification: p.variableParameterSpecification(s)}
	case PROCEDURE:
		p.err(p.c(), "procedure parameters not supported")
	case FUNCTION:
		p.err(p.c(), "functional parameters not supported")
	case IDENTIFIER:
		return &formalParameterSection{valueParameterSpecification: p.valueParameterSpecification(s)}
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
}

func (p *parser) variableParameterSpecification(s *scope) *variableParameterSpecification {
	return &variableParameterSpecification{
		p.mustShift(VAR),
		p.identifierList(),
		p.mustShift(':'),
		p.typeNode(s),
	}
}

// ValueParameterSpecification = IdentifierList ":" Type .
type valueParameterSpecification struct {
	list  []*tok
	colon *tok
	*typeNode
}

func (p *parser) valueParameterSpecification(s *scope) *valueParameterSpecification {
	return &valueParameterSpecification{
		p.identifierList(),
		p.mustShift(':'),
		p.typeNode(s),
	}
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
}

func (n *variableDeclaration) Position() token.Position { return n.list[0].Position() }

func (p *parser) variableDeclaration(s *scope) *variableDeclaration {
	r := &variableDeclaration{
		p.identifierList(),
		p.mustShift(':'),
		p.typeNode(s),
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

	r := &typeDefinition{
		ident:    p.mustShift(IDENTIFIER),
		eq:       p.mustShift('='),
		typeNode: p.typeNode(s),
	}
	s.declare(p, r, r.ident.src)
	r.typ = r.typeNode.typ
	if r.typ == nil {
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

func (p *parser) typeNode(s *scope) *typeNode {
	switch p.c().ch {
	case INT_LITERAL, IDENTIFIER, '-':
		r := &typeNode{simpleType: p.simpleType(s)}
		r.typ = r.simpleType.typ
		return r
	case PACKED, RECORD, FILE, ARRAY:
		r := &typeNode{structuredType: p.structuredType(s)}
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

func (p *parser) structuredType(s *scope) *structuredType {
	if p.c().ch == PACKED {
		r := &structuredType{
			packed:                 p.shift(),
			unpackedStructuredType: p.unpackedStructuredType(s),
		}
		r.typ = r.unpackedStructuredType.typ
		if r.typ != nil {
			r.typ.setPacked()
		}
		return r
	}

	r := &structuredType{
		unpackedStructuredType: p.unpackedStructuredType(s),
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

func (p *parser) unpackedStructuredType(s *scope) *unpackedStructuredType {
	switch p.c().ch {
	case FILE:
		r := &unpackedStructuredType{fileType: p.fileType(s)}
		r.typ = r.fileType.typ
		return r
	case RECORD:
		r := &unpackedStructuredType{recordType: p.recordType(s)}
		r.typ = r.recordType.typ
		return r
	case ARRAY:
		return &unpackedStructuredType{arrayType: p.arrayType(s)}
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
}

func (p *parser) arrayType(s *scope) *arrayType {
	return &arrayType{
		p.mustShift(ARRAY),
		p.mustShift('['),
		p.typeList(s),
		p.mustShift(']'),
		p.mustShift(OF),
		p.typeNode(s),
	}
}

func (p *parser) typeList(s *scope) (r []*typeNode) {
	r = append(r, p.typeNode(s))
	for p.c().ch == ',' {
		r = append(r, p.typeNode(s))
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

func (p *parser) recordType(s *scope) *recordType {
	r := &recordType{
		record:    p.mustShift(RECORD),
		fieldList: p.fieldList(s),
		end:       p.mustShift(END),
	}
	r.typ = newRecord(r.fieldList)
	return r
}

// FieldList = [ ( FixedPart [ ";" VariantPart ] | VariantPart ) [ ";" ] ] .
type fieldList struct {
	fixedPart []*recordSection
	*variantPart
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
	of   *tok
	list []*variant
}

func (p *parser) variantPart(s *scope) *variantPart {
	return &variantPart{
		p.mustShift(CASE),
		p.variantSelector(),
		p.mustShift(OF),
		p.variantList(s),
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
}

func (p *parser) variantSelector() *variantSelector {
	tok := p.mustShift(IDENTIFIER)
	switch p.c().ch {
	case ':':
		p.err(tok, "tag fields not supported")
	default:
		return &variantSelector{tagType: tok}
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

func (p *parser) recordSection(s *scope) *recordSection {
	r := &recordSection{
		list:     p.identifierList(),
		colon:    p.mustShift(':'),
		typeNode: p.typeNode(s),
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
		typeNode: p.typeNode(s),
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
			r := &simpleType{identifier: p.identifier(s)}
			switch x := r.identifier.n.(type) {
			case nil:
				// already reported
			case *typeDefinition:
				r.typ = x.typ
			case typ:
				r.typ = x
			default:
				p.err(r.identifier, "not a type: %s", r.identifier.src)
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
	switch x := r.first.op.(type) {
	case integerOperand:
		first = int(x)
	default:
		p.err(r.first, "expected integer constant")
	}
	switch x := r.last.op.(type) {
	case integerOperand:
		last = int(x)
	default:
		p.err(r.first, "expected integer constant")
	}
	r.typ = newSubrange(first, last)
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
	if r.op == nil || r.op.typ() == nil {
		p.err(r.ident, "constant value/type undetermined: %s", r.ident.src)
	}
	return r
}

// Constant = [Sign] ( UnsignedNumher | ConstantIdentifier) | CharacterString .
type constant struct {
	noder
	sign  *tok
	num   *tok
	ident *tok
	str   *tok
	op    operand
}

func (n *constant) resolveSymbol(p *parser, s *scope) {
	// const foo = bar;
	nm := n.ident.src
	_, nd := s.find(nm)
	switch x := nd.(type) {
	case nil:
		p.err(n.ident, "undefined: %s", nm)
	case *constantDefinition:
		n.op = x.constant.op
	default:
		p.err(n.ident, "not a constant: %s (%T)", nm, x)
	}
}

func (p *parser) constant(s *scope) (r *constant) {
	var err error
	sign := p.sign()
	if sign = p.sign(); sign != nil {
		sign = p.shift()
		switch p.c().ch {
		case INT_LITERAL:
			r := &constant{
				sign: sign,
				num:  p.shift(),
			}
			if r.op, err = newIntegerOperandFromString(r.num.src); err != nil {
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
		if r.op, err = newIntegerOperandFromString(r.num.src); err != nil {
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
			return &constant{
				str: str,
				op:  newStringOperand(goStringFromPascalString(str.src)),
			}
		}
	}

	p.err(p.c(), "expected constant")
	p.shift()
	return nil
}

func goStringFromPascalString(s string) string {
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

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

	return &parser{scanner: s}, nil
}

func (p *parser) err(n node, msg string, args ...interface{}) {
	pos := fmt.Sprintf("%v: ", n.Position())
	msg = fmt.Sprintf(pos+msg, args...)
	p.errs = append(p.errs, msg)
}

func (p *parser) c() (r *tok) {
	for {
		tok := p.c0()
		if tok.char != SEP {
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

func (p *parser) must(ch char) (tok *tok) {
	if tok = p.c(); tok.char == ch {
		return tok
	}

	p.err(tok, "%s, expected %s (%v)", tok, ch.str(), origin(3))
	return nil
}

func (p *parser) mustShift(ch char) *tok {
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
	if ex := m[nm]; ex != nil {
		p.err(n, "%s redeclared, previous declaration at %v:", nm, ex.Position())
		return
	}

	m[nm] = n
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
	if p.c().char == '(' {
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
	for p.c().char == ';' {
		p.shift()
		switch p.c().char {
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
	if p.c().char == INT_LITERAL {
		label = p.shift()
		p.mustShift(':')
	}
	switch p.c().char {
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
	switch p.c().char {
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
	switch p.c().char {
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
		expression: p.expression(),
		then:       p.mustShift(THEN),
		statement:  p.statement(s),
	}
	if p.c().char == ELSE {
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
		p.expression(),
		p.mustShift(OF),
		p.caseList(s),
		p.semiOpt(),
		p.mustShift(END),
	}
}

func (p *parser) semiOpt() *tok {
	if p.c().char == ';' {
		return p.shift()
	}

	return nil
}

func (p *parser) caseList(s *scope) (r []*case_) {
	r = []*case_{p.case_(s)}
	for p.c().char == ';' {
		semi := p.shift()
		if p.c().char == END {
			p.unget(semi)
			return r
		}

		r = append(r, p.case_(s))
	}
	return r
}

// Case = Constant { "," Constant } ":" Statement .
type case_ struct {
	list  []*constant
	colon *tok
	*statement
}

func (p *parser) case_(s *scope) *case_ {
	return &case_{
		p.constantList(),
		p.mustShift(':'),
		p.statement(s),
	}
}

func (p *parser) constantList() (r []*constant) {
	r = []*constant{p.constant()}
	for p.c().char == ',' {
		p.shift()
		r = append(r, p.constant())
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
	switch p.c().char {
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
		p.expression(),
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
		p.expression(),
		p.mustShift(DO),
		p.statement(s),
	}
}

// ForStatement = "for" ControlVariable ":=" InitialValue ( "to" | "downto" ) FinalValue "do" Statement .
type forStatement struct {
	for_       *tok
	ident      *tok
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
		p.mustShift(IDENTIFIER),
		p.mustShift(ASSIGN),
		p.expression(),
		p.toOrDownto(),
		p.expression(),
		p.mustShift(DO),
		p.statement(s),
	}
}

func (p *parser) toOrDownto() *tok {
	switch p.c().char {
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
	switch p.c().char {
	case ';', END:
		return &simpleStatement{}
	case GOTO:
		return &simpleStatement{gotoStatement: p.gotoStatement()}
	case IDENTIFIER:
		id := p.shift()
		switch p.c().char {
		case '(', ';', ELSE, END, UNTIL:
			p.unget(id)
			return &simpleStatement{procedureStatement: p.procedureStatement()}
		}

		p.unget(id)
		return &simpleStatement{assignmentStatement: p.assignmentStatement()}
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

func (p *parser) procedureStatement() *procedureStatement {
	return &procedureStatement{
		p.mustShift(IDENTIFIER),
		p.argList(),
	}
}

func (p *parser) argList() (r []*arg) {
	if p.c().char != '(' {
		return nil
	}

	p.shift()
	r = []*arg{p.arg()}
	for p.c().char == ',' {
		p.shift()
		r = append(r, p.arg())
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

func (p *parser) arg() *arg {
	r := &arg{expression: p.expression()}
	if p.c().char == ':' {
		r.colon = p.shift()
		r.width = p.expression()
		if p.c().char == ':' {
			r.colon2 = p.shift()
			r.width2 = p.expression()
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

func (p *parser) assignmentStatement() *assignmentStatement {
	return &assignmentStatement{
		p.variable(),
		p.mustShift(ASSIGN),
		p.expression(),
	}
}

// Expression = SimpleExression [ RelationalOperator SimpleExression ] .
type expression struct {
	*simpleExpression
	relOp *tok
	rhs   *simpleExpression
}

func (p *parser) expression() *expression {
	r := &expression{simpleExpression: p.simpleExpression()}
	switch p.c().char {
	case '=', NE, '<', LE, '>', GE, IN:
		r.relOp = p.shift()
		r.rhs = p.simpleExpression()
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
func (p *parser) term() *term {
	r := &term{factor: p.factor()}
	for {
		switch p.c().char {
		case '*', '/', DIV, MOD, AND:
			r.list = append(r.list, termListItem{
				p.shift(),
				p.factor(),
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

func (p *parser) factor() *factor {
	switch p.c().char {
	case INT_LITERAL, STR_LITERAL, REAL_LITERAL:
		return &factor{unsignedConstant: p.unsignedConstant()}
	case IDENTIFIER:
		id := p.shift()
		if p.c().char == '(' {
			p.unget(id)
			return &factor{functionDesignator: p.functionDesignator()}
		}

		p.unget(id)
		return &factor{variable: p.variable()}
	case '(':
		p.shift()
		r := &factor{expression: p.expression()}
		p.mustShift(')')
		return r
	case NOT:
		p.shift()
		return &factor{not: p.factor()}
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

func (p *parser) functionDesignator() *functionDesignator {
	return &functionDesignator{
		p.mustShift(IDENTIFIER),
		p.actualParameterList(),
	}
}

func (p *parser) actualParameterList() (r []*expression) {
	if p.c().char != '(' {
		return nil
	}

	p.shift()
	r = p.expressionList()
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
	switch p.c().char {
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
	switch p.c().char {
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

func (p *parser) simpleExpression() *simpleExpression {
	sign := p.sign()
	r := &simpleExpression{
		sign: sign,
		term: p.term(),
	}
	for {
		switch p.c().char {
		case '+', '-', OR:
			r.list = append(r.list, simpleExpressionListItem{
				p.shift(),
				p.term(),
			})
		default:
			return r
		}
	}
}

func (p *parser) expressionList() (r []*expression) {
	r = []*expression{p.expression()}
	for p.c().char == ',' {
		p.shift()
		r = append(r, p.expression())
	}
	return r
}

// Variable = EntireVariable | ComponentVariable |
//	IdentifiedVariable | BufferVariable .
type variable struct {
	entire *tok
	*componentVariable
	deref *variable
}

func (p *parser) variable() (r *variable) {
	switch p.c().char {
	case IDENTIFIER:
		r = &variable{entire: p.shift()}
		for {
			switch p.c().char {
			case '[', '.':
				r = &variable{componentVariable: p.componentVariable(r)}
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

func (p *parser) componentVariable(v *variable) *componentVariable {
	switch p.c().char {
	case '[':
		return &componentVariable{indexedVariable: p.indexedVariable(v)}
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
		p.mustShift(IDENTIFIER),
	}
}

// IndexedVariahle = ArrayVariable "[" Index { "," Index } "]" .
type indexedVariable struct {
	*variable
	lbrace *tok
	list   []*expression
	rbrace *tok
}

func (p *parser) indexedVariable(v *variable) *indexedVariable {
	return &indexedVariable{
		v,
		p.mustShift('['),
		p.expressionList(),
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
		switch p.c().char {
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

func (p *parser) functionDeclaration(s *scope) *functionDeclaration {
	r := &functionDeclaration{scope: scope{parent: s}}
	r.functionHeading = p.functionHeading()
	r.semi = p.mustShift(';')
	if t := p.c(); t.char == IDENTIFIER && strings.ToLower(t.src) == "forward" {
		r.forward = p.shift()
		return r
	}

	r.block = p.block(&r.scope)
	s.declare(p, r, r.functionHeading.ident.src)
	return r
}

// FunctionHeading = "function" Identifier [ FormalParameterList ] ":" Type .
type functionHeading struct {
	function *tok
	ident    *tok
	list     []*formalParameterSection
	colon    *tok
	typ      *typ
}

func (p *parser) functionHeading() *functionHeading {
	return &functionHeading{
		p.mustShift(FUNCTION),
		p.mustShift(IDENTIFIER),
		p.formalParameterList(),
		p.mustShift(':'),
		p.typ(),
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

func (p *parser) procedureDeclaration(s *scope) *procedureDeclaration {
	r := &procedureDeclaration{scope: scope{parent: s}}
	r.procedureHeading = p.procedureHeading()
	r.semi = p.mustShift(';')
	if t := p.c(); t.char == IDENTIFIER && strings.ToLower(t.src) == "forward" {
		r.forward = p.shift()
		return r
	}

	r.block = p.block(&r.scope)
	s.declare(p, r, r.procedureHeading.ident.src)
	return r
}

// ProcedureHeading = "procedure" Identifier [ FormalParameterList ] .
type procedureHeading struct {
	procedure *tok
	ident     *tok
	list      []*formalParameterSection
}

func (p *parser) procedureHeading() *procedureHeading {
	return &procedureHeading{
		p.mustShift(PROCEDURE),
		p.mustShift(IDENTIFIER),
		p.formalParameterList(),
	}
}

func (p *parser) formalParameterList() (r []*formalParameterSection) {
	if p.c().char != '(' {
		return nil
	}

	p.shift()
	r = []*formalParameterSection{p.formalParameterSection()}
	for p.c().char == ';' {
		p.shift()
		r = append(r, p.formalParameterSection())
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

func (p *parser) formalParameterSection() *formalParameterSection {
	switch p.c().char {
	case VAR:
		return &formalParameterSection{variableParameterSpecification: p.variableParameterSpecification()}
	case PROCEDURE:
		p.err(p.c(), "procedure parameters not supported")
	case FUNCTION:
		p.err(p.c(), "functional parameters not supported")
	case IDENTIFIER:
		return &formalParameterSection{valueParameterSpecification: p.valueParameterSpecification()}
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
	*typ
}

func (p *parser) variableParameterSpecification() *variableParameterSpecification {
	return &variableParameterSpecification{
		p.mustShift(VAR),
		p.identifierList(),
		p.mustShift(':'),
		p.typ(),
	}
}

// ValueParameterSpecification = IdentifierList ":" Type .
type valueParameterSpecification struct {
	list  []*tok
	colon *tok
	*typ
}

func (p *parser) valueParameterSpecification() *valueParameterSpecification {
	return &valueParameterSpecification{
		p.identifierList(),
		p.mustShift(':'),
		p.typ(),
	}
}

// VariableDeclarationPart = [ "var" VariableDeclaration ";" { VariableDeclaration ";" } ] .
type variableDeclarationPart struct {
	var_ *tok
	list []*variableDeclaration
}

func (p *parser) variableDeclarationPart(s *scope) *variableDeclarationPart {
	if p.c().char != VAR {
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
	for p.c().char == IDENTIFIER {
		r = append(r, p.variableDeclaration(s))
		p.mustShift(';')
	}
	return r
}

// VariableDeclaration = IdentifierList ":" Type .
type variableDeclaration struct {
	list  []*tok
	colon *tok
	typ   *typ
}

func (n *variableDeclaration) Position() token.Position { return n.list[0].Position() }

func (p *parser) variableDeclaration(s *scope) *variableDeclaration {
	r := &variableDeclaration{
		p.identifierList(),
		p.mustShift(':'),
		p.typ(),
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
	if p.c().char != TYPE {
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
	typ   *typ
}

func (n *typeDefinition) Position() token.Position { return n.ident.Position() }

func (p *parser) typeDefinition(s *scope) *typeDefinition {
	if p.c().char != IDENTIFIER {
		return nil
	}

	r := &typeDefinition{
		p.mustShift(IDENTIFIER),
		p.mustShift('='),
		p.typ(),
	}
	s.declare(p, r, r.ident.src)
	return r
}

// Type = SimpleType | StructuredType | PointerType .
type typ struct {
	*simpleType
	*structuredType
}

func (p *parser) typ() *typ {
	switch p.c().char {
	case INT_LITERAL, IDENTIFIER, '-':
		return &typ{simpleType: p.simpleType()}
	case PACKED, RECORD, FILE, ARRAY:
		return &typ{structuredType: p.structuredType()}
	}
	p.err(p.c(), "unexpected %q, expected simple type or structured type or pointer type", p.c().src)
	p.shift()
	return nil
}

// StructuredType = [ "packed" ] UnpackedStructuredType | StructuredTypeIdentifier .
type structuredType struct {
	packed *tok
	*unpackedStructuredType
}

func (p *parser) structuredType() *structuredType {
	if p.c().char == PACKED {
		return &structuredType{
			packed:                 p.shift(),
			unpackedStructuredType: p.unpackedStructuredType(),
		}
	}

	return &structuredType{
		unpackedStructuredType: p.unpackedStructuredType(),
	}
}

// UnpackedStructuredType = ArrayType | RecordType | SetType | FileType .
type unpackedStructuredType struct {
	*arrayType
	*fileType
	*recordType
}

func (p *parser) unpackedStructuredType() *unpackedStructuredType {
	switch p.c().char {
	case FILE:
		return &unpackedStructuredType{fileType: p.fileType()}
	case RECORD:
		return &unpackedStructuredType{recordType: p.recordType()}
	case ARRAY:
		return &unpackedStructuredType{arrayType: p.arrayType()}
	}
	p.err(p.c(), "unexpected %q, expected arary type or record type or set type or file type", p.c().src)
	p.shift()
	return nil
}

// ArrayType = "array" "[" IndexType { "," IndexType } "]" "of" ComponentType .
type arrayType struct {
	array  *tok
	lbrace *tok
	list   []*typ
	rbrace *tok
	of     *tok
	typ    *typ
}

func (p *parser) arrayType() *arrayType {
	return &arrayType{
		p.mustShift(ARRAY),
		p.mustShift('['),
		p.typeList(),
		p.mustShift(']'),
		p.mustShift(OF),
		p.typ(),
	}
}

func (p *parser) typeList() (r []*typ) {
	r = append(r, p.typ())
	for p.c().char == ',' {
		r = append(r, p.typ())
	}
	return r
}

// RecordType = "record" FieldList "end" .
type recordType struct {
	record *tok
	*fieldList
	end *tok
}

func (p *parser) recordType() *recordType {
	return &recordType{
		p.mustShift(RECORD),
		p.fieldList(),
		p.mustShift(END),
	}
}

// FieldList = [ ( FixedPart [ ";" VariantPart ] | VariantPart ) [ ";" ] ] .
type fieldList struct {
	fixedPart []*recordSection
	*variantPart
}

func (p *parser) fieldList() *fieldList {
	switch p.c().char {
	case IDENTIFIER:
		r := &fieldList{fixedPart: p.fixedPart()}
		switch p.c().char {
		case ';':
			p.shift()
			switch p.c().char {
			case CASE:
				r.variantPart = p.variantPart()
				if p.c().char == ';' {
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
		r := &fieldList{variantPart: p.variantPart()}
		if p.c().char == ';' {
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

func (p *parser) variantPart() *variantPart {
	return &variantPart{
		p.mustShift(CASE),
		p.variantSelector(),
		p.mustShift(OF),
		p.variantList(),
	}
}

// Variant { ";" Variant }
func (p *parser) variantList() (r []*variant) {
	v := p.variant()
	if v == nil {
		return nil
	}

	r = []*variant{v}
	for p.c().char == ';' {
		semi := p.shift()
		if p.c().char == END {
			p.unget(semi)
			break
		}

		r = append(r, p.variant())
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

func (p *parser) variant() *variant {
	c := p.constant()
	if c == nil {
		return nil
	}

	r := &variant{consts: []*constant{c}}
	for p.c().char == ',' {
		p.shift()
		r.consts = append(r.consts, p.constant())
	}
	r.colon = p.mustShift(':')
	r.lparen = p.mustShift('(')
	r.fieldList = p.fieldList()
	r.rparen = p.mustShift(')')
	return r
}

// VariantSelector = [ TagField ":"] TagType .
type variantSelector struct {
	tag   *tok
	colon *tok
	typ   *tok
}

func (p *parser) variantSelector() *variantSelector {
	tok := p.mustShift(IDENTIFIER)
	switch p.c().char {
	case ':':
		p.err(tok, "tag fields not supported")
	default:
		return &variantSelector{typ: tok}
	}
	p.shift()
	return nil
}

// FixedPart = RecordSection { ";" RecordSection } .
func (p *parser) fixedPart() []*recordSection {
	rs := p.recordSection()
	if rs == nil {
		return nil
	}

	r := []*recordSection{rs}
	for p.c().char == ';' {
		semi := p.shift()
		if ch := p.c().char; ch == CASE || ch == END {
			p.unget(semi)
			break
		}

		if rs = p.recordSection(); rs == nil {
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
	typ   *typ
}

func (p *parser) recordSection() *recordSection {
	return &recordSection{
		p.identifierList(),
		p.mustShift(':'),
		p.typ(),
	}
}

// IdentifierList = Identifier { "," Identifier } .
func (p *parser) identifierList() []*tok {
	t := p.mustShift(IDENTIFIER)
	if t == nil {
		return nil
	}

	r := []*tok{t}
	for p.c().char == ',' {
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
	*typ
}

func (p *parser) fileType() *fileType {
	return &fileType{
		p.mustShift(FILE),
		p.mustShift(OF),
		p.typ(),
	}
}

// SimpleType = OrdinalType | RealTypeldentifier.
type simpleType struct {
	*ordinalType
	ident *tok
}

func (p *parser) simpleType() *simpleType {
	switch p.c().char {
	case INT_LITERAL, '-':
		return &simpleType{ordinalType: p.ordinalType()}
	case IDENTIFIER:
		id := p.shift()
		if p.c().char != DD {
			return &simpleType{ident: id}
		}

		p.unget(id)
		return &simpleType{ordinalType: p.ordinalType()}
	}
	p.err(p.c(), "unexpected %q, expected ordinal type or real type identifier", p.c().src)
	p.shift()
	return nil
}

// OrdinalType = EnumeratedType | SubrangeType | OrdinalTypeldentifier .
type ordinalType struct {
	*subrangeType
	ident *tok
}

func (p *parser) ordinalType() *ordinalType {
	switch p.c().char {
	case INT_LITERAL, '-':
		return &ordinalType{subrangeType: p.subrangeType()}
	case IDENTIFIER:
		id := p.shift()
		if p.c().char != DD {
			return &ordinalType{ident: id}
		}

		p.unget(id)
		return &ordinalType{subrangeType: p.subrangeType()}
	}
	p.err(p.c(), "unexpected %q, enumerated type or subrange type or ordinal type identifier", p.c().src)
	p.shift()
	return nil
}

// SubrangeType = Constant ".." Constant .
type subrangeType struct {
	first *constant
	dd    *tok
	last  *constant
}

func (p *parser) subrangeType() *subrangeType {
	return &subrangeType{
		p.constant(),
		p.mustShift(DD),
		p.constant(),
	}
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
	if p.c().char != CONST {
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
	if p.c().char != IDENTIFIER {
		return nil
	}

	r := &constantDefinition{
		p.shift(),
		p.mustShift('='),
		p.constant(),
	}
	s.declare(p, r, r.ident.src)
	return r
}

// Constant = [Sign] ( UnsignedNumher | ConstantIdentifier) | CharacterString .
type constant struct {
	sign  *tok
	num   *tok
	ident *tok
	str   *tok
}

func (p *parser) constant() *constant {
	sign := p.sign()
	if sign = p.sign(); sign != nil {
		sign = p.shift()
		switch p.c().char {
		case INT_LITERAL:
			return &constant{
				sign: sign,
				num:  p.shift(),
			}
		case IDENTIFIER:
			return &constant{
				sign:  sign,
				ident: p.shift(),
			}
		default:
			p.err(p.c(), "expected unsigned number or identifier")
			p.shift()
			return nil
		}
	}

	switch p.c().char {
	case INT_LITERAL:
		return &constant{
			num: p.shift(),
		}
	case IDENTIFIER:
		return &constant{
			ident: p.shift(),
		}
	case STR_LITERAL:
		return &constant{
			str: p.shift(),
		}
	}
	p.err(p.c(), "expected constant")
	p.shift()
	return nil
}

func (p *parser) sign() *tok {
	switch p.c().char {
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
	if p.c().char != LABEL {
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
	for p.c().char == ',' {
		p.shift()
		tok := p.mustShift(INT_LITERAL)
		if tok == nil {
			break
		}

		r = append(r, tok)
	}
	return r
}

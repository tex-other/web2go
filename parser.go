// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"fmt"
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
	tok      *tok // current token, nil initially and if consumed by shift()
	ungetBuf *tok

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
			panic(todo("%q", v))
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
//	nil		nil		internal error
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

	panic(todo("internal error"))
}

// 	tok		ungetBuf	action
//	---		--------	------
//	nil		nil		p.ungetBuf = tok
//	nil		non-nil		p.tok = p.ungetBuf; p.ungetBuf = tok
//	non-nil		nil		p.ungetBuf = tok
//	non-nil		non-nil		internal error
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

	panic(todo("internal error"))
}

func (p *parser) must(ch char) (tok *tok) {
	if tok = p.c(); tok.char == ch {
		return tok
	}

	p.err(tok, "%s, expected %s (%v:)", tok, ch.str(), origin(3))
	return nil
}

func (p *parser) mustShift(ch char) *tok {
	tok := p.must(ch)
	p.shift()
	return tok
}

// Program = ProgramHeading ";" Block "." .
type program struct {
	*programHeading
	semi *tok
	*block
	dot *tok
}

func (p *parser) program() *program {
	return &program{
		p.programHeading(),
		p.mustShift(';'),
		p.block(),
		p.mustShift('.'),
	}
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
func (p *parser) block() *block {
	return &block{
		p.labelDeclarationPart(),
		p.constantDefinitionPart(),
		p.typeDefinitionPart(),
		p.variableDeclarationPart(),
		p.procedureAndFunctionDeclarationPart(),
		p.compoundStatement(),
	}
}

// CompoundStatement = "begin" StatementSequence "end" .
type compoundStatement struct {
	begin *tok
	list  []*statement
	end   *tok
}

func (p *parser) compoundStatement() *compoundStatement {
	return &compoundStatement{
		p.mustShift(BEGIN),
		p.statementList(),
		p.mustShift(END),
	}
}

func (p *parser) statementList() (r []*statement) {
	panic(todo("", p.c()))
}

type statement struct{}

func (p *parser) statment() *statement {
	panic(todo("", p.c()))
}

// ProcedureAndFunctionDeclarationPart = { (ProcedureDeclaration | FunctionDeclaration ) ";" } .
type procedureAndFunctionDeclarationPart struct {
	*procedureDeclaration
}

func (p *parser) procedureAndFunctionDeclarationPart() (r []*procedureAndFunctionDeclarationPart) {
	for {
		switch p.c().char {
		case PROCEDURE:
			r = append(r, &procedureAndFunctionDeclarationPart{procedureDeclaration: p.procedureDeclaration()})
		case FUNCTION:
			panic(todo("", p.c()))
		default:
			return r
		}
	}
}

// ProcedureDeclaration = ProcedureHeading ";" Block |
//	ProcedureHeading ";" Directive |
//	ProcedureIdentification ";" Block.
type procedureDeclaration struct {
	*procedureHeading
	semi *tok
	*block
}

func (p *parser) procedureDeclaration() *procedureDeclaration {
	return &procedureDeclaration{
		p.procedureHeading(),
		p.mustShift(';'),
		p.block(),
	}
}

// ProcedureHeading = "procedure" Identifier [ FormaIParameterList ] .
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

	panic(todo("", p.c()))
}

type formalParameterSection struct{}

func (p *parser) formalParameterSection() *formalParameterSection {
	panic(todo("", p.c()))
}

// VariableDeclarationPart = [ "var" VariableDeclaration ";" { VariableDeclaration ";" } ] .
type variableDeclarationPart struct {
	var_ *tok
	list []*variableDeclaration
}

func (p *parser) variableDeclarationPart() *variableDeclarationPart {
	if p.c().char != VAR {
		return nil
	}

	return &variableDeclarationPart{
		p.shift(),
		p.variableDeclarationList(),
	}
}

func (p *parser) variableDeclarationList() (r []*variableDeclaration) {
	r = append(r, p.variableDeclaration())
	p.mustShift(';')
	for p.c().char == IDENTIFIER {
		r = append(r, p.variableDeclaration())
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

func (p *parser) variableDeclaration() *variableDeclaration {
	return &variableDeclaration{
		p.identifierList(),
		p.mustShift(':'),
		p.typ(),
	}
}

// TypeDefinitionPart = [ "type" TypeDefinition ";" { TypeDefinitionPart ";" } ] .
type typeDefinitionPart struct {
	typ  *tok
	list []*typeDefinition
}

func (p *parser) typeDefinitionPart() *typeDefinitionPart {
	if p.c().char != TYPE {
		return nil
	}

	return &typeDefinitionPart{
		p.shift(),
		p.typeDefinitionList(),
	}
}

// TypeDefinition = Identifier "=" Type .
type typeDefinition struct {
	ident *tok
	eq    *tok
	typ   *typ
}

func (p *parser) typeDefinition() *typeDefinition {
	if p.c().char != IDENTIFIER {
		return nil
	}

	return &typeDefinition{
		p.mustShift(IDENTIFIER),
		p.mustShift('='),
		p.typ(),
	}
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
	default:
		p.err(p.c(), "unexpected %q, expected simple type or structured type or pointer type", p.c().src)
	}
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
	default:
		p.err(p.c(), "unexpected %q, expected arary type or record type or set type or file type", p.c().src)
	}
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
				_ = r
				panic(todo("", p.c()))
			}
		case ')':
			return r
		default:
			panic(todo("", p.c()))
		}
	case CASE:
		r := &fieldList{variantPart: p.variantPart()}
		if p.c().char == ';' {
			p.shift()
		}
		return r
	default:
		p.err(p.c(), "unexpected %q, expected fixed part or variant part", p.c().src)
	}
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
		panic(todo("", p.c()))
	default:
		return &variantSelector{typ: tok}
	}
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
	default:
		p.err(p.c(), "unexpected %q, expected ordinal type or real type identifier", p.c().src)
	}
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
	default:
		p.err(p.c(), "unexpected %q, enumerated type or subrange type or ordinal type identifier", p.c().src)
	}
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
func (p *parser) typeDefinitionList() (r []*typeDefinition) {
	td := p.typeDefinition()
	if td == nil {
		return nil
	}

	p.mustShift(';')
	r = []*typeDefinition{td}
	for {
		td = p.typeDefinition()
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

func (p *parser) constantDefinitionPart() *constantDefinitionPart {
	if p.c().char != CONST {
		return nil
	}

	return &constantDefinitionPart{
		p.shift(),
		p.constantDefinitionList(),
	}
}

// ConstantDefinition = Identifier "=" Constant .
type constantDefinition struct {
	ident *tok
	eq    *tok
	*constant
}

func (p *parser) constantDefinition() *constantDefinition {
	if p.c().char != IDENTIFIER {
		return nil
	}

	return &constantDefinition{
		p.shift(),
		p.mustShift('='),
		p.constant(),
	}
}

// Constant = [Sign] ( UnsignedNumher | ConstantIdentifier) | CharacterString .
type constant struct {
	sign  *tok
	num   *tok
	ident *tok
	str   *tok
}

func (p *parser) constant() *constant {
	var sign *tok
	if p.c().char == '-' {
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
	default:
		p.err(p.c(), "expected constant")
		return nil
	}
}

// ConstantDefinition ";" { ConstantDefinition ";" }
func (p *parser) constantDefinitionList() (r []*constantDefinition) {
	cd := p.constantDefinition()
	if cd == nil {
		return nil
	}

	p.mustShift(';')
	r = []*constantDefinition{cd}
	for {
		cd = p.constantDefinition()
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

func (p *parser) labelDeclarationPart() *labelDeclarationPart {
	if p.c().char != LABEL {
		return nil
	}

	return &labelDeclarationPart{
		p.shift(),
		p.labelList(),
		p.mustShift(';'),
	}
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

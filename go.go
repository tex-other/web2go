// Copyright 2021 The web2go Authors. All rights reserved.
// Use of this source code is governed by a BSD-style
// license that can be found in the LICENSE file.

package main // import "modernc.org/web2go"

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"os/exec"
	"strings"
)

type project struct {
	task *task
	out  io.Writer
	errs []string

	closed bool
}

func newProject(t *task) *project {
	return &project{
		task: t,
	}
}

func (p *project) err(n node, msg string, args ...interface{}) {
	if !p.task.e && len(p.errs) == 10 {
		return
	}

	pos := "-: "
	if n != nil {
		pos = fmt.Sprintf("%v: ", n.Position())
	}
	msg = fmt.Sprintf(pos+msg, args...)
	if s := strings.ToLower(msg); strings.Contains(s, "internal error") || strings.Contains(s, "todo") {
		msg += fmt.Sprintf(" (%v)", origin(2))
	}
	p.errs = append(p.errs, msg)
}

func (p *project) w(s string, args ...interface{}) {
	if p.closed {
		return
	}

	if _, err := fmt.Fprintf(p.out, s, args...); err != nil {
		p.err(nil, "%s", err)
		p.closed = true
	}
}

func (p *project) main(program *program) (err error) {
	defer func() {
		if len(p.errs) != 0 {
			err = fmt.Errorf("%s", strings.Join(p.errs, "\n"))
		}
	}()

	f, err := os.Create(p.task.o)
	if err != nil {
		p.err(nil, "%s", err)
		return
	}

	defer func() {
		if err := f.Close(); err != nil {
			p.err(nil, "%s", err)
			return
		}

		if out, err := exec.Command("gofmt", "-s", "-w", p.task.o).CombinedOutput(); err != nil {
			p.err(nil, "%s\n%s", out, err)
		}
	}()

	buf := bufio.NewWriter(f)

	defer func() {
		if err := buf.Flush(); err != nil {
			p.err(nil, "%s", err)
		}
	}()

	p.out = buf
	p.w("// Code generated by %v, DO NOT EDIT.\n\n", p.task.args)
	for _, v := range p.task.copyright {
		p.w("\n// %s", v)
	}
	p.w(`
package %s

import "unsafe"

`, p.task.pkgName)
	p.block(program.block, true)
	rtl := assets["/rtl.go"]
	const tag = "/* CUT HERE */"
	rtl = rtl[strings.Index(rtl, tag)+len(tag):]
	rtl = strings.ReplaceAll(rtl, "TYPE", "tex")
	p.w("%s", rtl)
	return
}

func (p *project) block(n *block, tld bool) {
	p.constantDefinitionPart(n.constantDefinitionPart)
	p.typeDefinitionPart(n.typeDefinitionPart)
	p.variableDeclarationPart(n.variableDeclarationPart, tld)
	for _, v := range n.list {
		p.procedureAndFunctionDeclarationPart(v)
	}
}

func (p *project) procedureAndFunctionDeclarationPart(n *procedureAndFunctionDeclarationPart) {
	if n.procedureDeclaration != nil {
		p.procedureDeclaration(n.procedureDeclaration)
		return
	}

	if n.functionDeclaration != nil {
		p.functionDeclaration(n.functionDeclaration)
		return
	}
}

func (p *project) functionDeclaration(n *functionDeclaration) {
	if n.forward != nil {
		return
	}

	p.w("\n\nfunc (%s *%s) %s(", p.task.rcvrName, p.task.progTypeName, p.ident(n.functionHeading.ident.src))
	p.formalParameters(n.functionHeading.list)
	p.w(") %s { panic(\"TODO\") }", n.functionHeading.typ.goType())
}

func (p *project) formalParameters(list []*formalParameterSection) {
	for _, v := range list {
		var list []*tok
		prefix := ""
		switch {
		case v.valueParameterSpecification != nil:
			list = v.valueParameterSpecification.list
		case v.variableParameterSpecification != nil:
			list = v.variableParameterSpecification.list
			if _, ok := v.typ.(*file); !ok {
				prefix = "*"
			}
		}
		for i, v := range list {
			p.w("%s", v.src)
			if i != len(list)-1 {
				p.w(", ")
			}
		}
		p.w(" %s%s", prefix, v.typ.goType())
		p.w(", ")
	}
}

func (p *project) ident(s string) string {
	switch s {
	case "package":
		return "package1"
	default:
		return s
	}
}

func (p *project) procedureDeclaration(n *procedureDeclaration) {
	if n.forward != nil {
		return
	}

	p.w("\n\nfunc (%s *%s) %s(", p.task.rcvrName, p.task.progTypeName, p.ident(n.procedureHeading.ident.src))
	p.formalParameters(n.procedureHeading.list)
	p.w(") { panic(\"TODO\") }")
}

func (p *project) variableDeclarationPart(n *variableDeclarationPart, tld bool) {
	prefix := "var "
	if tld {
		prefix = ""
		p.w("\n\ntype %s struct{", p.task.progTypeName)
		defer p.w("\n}")
	}
	for _, v := range n.list {
		for _, w := range v.list {
			p.w("\n%s%s %s", prefix, w.src, v.typ.goType())
		}
	}
}

func (p *project) typeDefinitionPart(n *typeDefinitionPart) {
	if n == nil {
		return
	}

	var recs []*typeDefinition
	p.w("\n\ntype (")
	for _, v := range n.list {
		switch v.typ.(type) {
		case *record:
			recs = append(recs, v)
			continue
		}

		p.w("\n\t%s %s", v.ident.src, v.typ.render())
	}
	p.w("\n)\n")
	for _, v := range recs {
		p.record(v)
	}
}

func (p *project) record(td *typeDefinition) {
	tn := td.ident.src
	p.w("\ntype %s %s\n", tn, td.typ.render())
	for _, v := range td.typ.(*record).variants {
		fn := v.name
		ft := v.typ.goType()
		switch {
		case v.off == 0:
			p.w("\n\nfunc (r *%s) %s() %s { return *(*%[3]s)(unsafe.Pointer(&r.variant)) }", tn, fn, ft)
			p.w("\n\nfunc (r *%s) %s(v %s) { *(*%[3]s)(unsafe.Pointer(&r.variant)) = v }", tn, p.setterName(fn), ft)
		default:
			p.w("\n\nfunc (r *%s) %s() %s { return *(*%[3]s)(unsafe.Pointer(uintptr(unsafe.Pointer(&r.variant))+%d)) }", tn, fn, ft, v.off)
			p.w("\n\nfunc (r *%s) %s(v %s) { *(*%[3]s)(unsafe.Pointer(uintptr(unsafe.Pointer(&r.variant))+%d)) = v }", tn, p.setterName(fn), ft, v.off)
		}
	}
}

func (p *project) setterName(s string) string {
	return "set" + strings.ToUpper(s[:1]) + s[1:]
}

func (p *project) constantDefinitionPart(n *constantDefinitionPart) {
	if n == nil {
		return
	}

	p.w("\n\nconst (")
	for _, v := range n.list {
		p.w("\n\t%s = %s", v.ident.src, v.literal.render())
	}
	p.w("\n)")
}

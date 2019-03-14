package parser

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/volts-dev/eval/ast"
	"github.com/volts-dev/lexer"
)

type (
	// TokenFn
	prefixParseFn func() ast.Expr
	//
	infixParseFn func(ast.Expr) ast.Expr

	// Parser 提供解析的逻辑 和数据的变换
	TParser struct {
		name string
		//template *TTemplate
		parent *TParser
		lexer  *lexer.TLexer
		//root   *nodeDocument // 解析后的Node
		//silo []interface{}

		pos       int // the pos of token
		tokens    []*lexer.TToken
		lastToken *lexer.TToken

		// Non-syntactic parser control
		exprLev int  // < 0: in control clause, >= 0: in expression
		inRhs   bool //  Right Hand Side if set, the parser is parsing a rhs expression

	}
)

// #新建解析器可以独立于TTemplate使用
func NewParser() *TParser {
	parse := &TParser{
		//template: aTemplete,
		parent: nil,
		//		lex:      lLexer,
		//silo: []interface{}{}
	}

	return parse
}

// Returns tokens[i] or NIL (if i >= len(tokens))
func (p *TParser) Get(i int) *lexer.TToken {
	if i < len(p.tokens) {
		//fmt.Println("Get", i, p.tokens[i])
		return p.tokens[i]
	}
	return nil
}

// Consume one token. It will be gone forever.
func (p *TParser) next() {
	p.nextn(1)
}

// Consume N tokens. They will be gone forever.
func (p *TParser) nextn(count int) {
	p.pos += count
}

func (p *TParser) skipWhitespace() {
	for {
		if p.Current().Type != lexer.SAPCE {
			return
		}
		fmt.Println(lexer.TokenNames[p.Current().Type])
		p.next()
	}

	return
}

// Returns the UNCONSUMED token count.
func (p *TParser) Remaining() int {
	return len(p.tokens) - p.pos
}

// Returns the current token.
func (p *TParser) Current() *lexer.TToken {
	return p.Get(p.pos)
}

// Returns the prior token.
func (p *TParser) Prior() *lexer.TToken {
	return p.Get(p.pos - 1)
}

// Returns the CURRENT token if the given type AND value matches.
// It DOES NOT consume the token.
func (p *TParser) Peek(typ int, val string) *lexer.TToken {
	return p.PeekN(0, typ, val)
}

// Returns the tokens[current position + shift] token if the
// given type AND value matches for that token.
// DOES NOT consume the token.
func (p *TParser) PeekN(shift int, typ int, val string) *lexer.TToken {
	t := p.Get(p.pos + shift)
	if t != nil {
		//fmt.Println("PeekN", t)
		if t.Type == typ && t.Val == val {
			return t
		}
	}
	return nil
}

// Returns the CURRENT token if the given type matches.
// It DOES NOT consume the token.
func (p *TParser) PeekType(typ int) *lexer.TToken {
	return p.PeekTypeN(0, typ)
}

// Returns the tokens[current position + shift] token if the given type matches.
// DOES NOT consume the token for that token.
func (p *TParser) PeekTypeN(shift int, typ int) *lexer.TToken {
	t := p.Get(p.pos + shift)
	if t != nil {
		//fmt.Println("PeekTypeN", t)
		if t.Type == typ {
			return t
		}
	}
	return nil
}

// If x is of the form (T), unparen returns unparen(T), otherwise it returns x.
func unparen(x ast.Expr) ast.Expr {
	//if p, isParen := x.(*ast.ParenExpr); isParen {
	//	x = unparen(p.X)
	//}
	return x
}

// isTypeName reports whether x is a (qualified) TypeName.
func isTypeName(x ast.Expr) bool {
	switch t := x.(type) {
	case *ast.TBadExpr:
	case *ast.TIdentExpr:
	case *ast.TSelectorExpr:
		_, isIdent := t.X.(*ast.TIdentExpr)
		return isIdent
	default:
		return false // all other nodes are not type names
	}
	return true
}

func isKeyword(key string) bool {
	for _, k := range ast.Keywords {
		if k == key {
			return true
		}
	}
	return false
}

// isLiteralType reports whether x is a legal composite literal type.
func isLiteralType(x ast.Expr) bool {
	switch t := x.(type) {
	case *ast.TBadExpr:
	case *ast.TIdentExpr:
	case *ast.TSelectorExpr:
		_, isIdent := t.X.(*ast.TIdentExpr)
		return isIdent
	//case *ast.ArrayType:
	//case *ast.StructType:
	//case *ast.MapType:
	default:
		return false // all other nodes are not legal composite literal types
	}
	return true
}

// A bailout panic is raised to indicate early termination.
type bailout struct{}

func (p *TParser) error(pos int, msg string) {
	/*epos := p.file.Position(pos)

	// If AllErrors is not set, discard errors reported on the same line
	// as the last recorded error and stop parsing if there are more than
	// 10 errors.
	if p.mode&AllErrors == 0 {
		n := len(p.errors)
		if n > 0 && p.errors[n-1].Pos.Line == epos.Line {
			return // discard - likely a spurious error
		}
		if n > 10 {
			panic(bailout{})
		}
	}
	*/
	//p.errors.Add(epos, msg)
	fmt.Println(msg)
}

func (p *TParser) errorExpected(pos int, msg string) {
	msg = "expected " + msg
	t := p.Current()
	if pos == t.Type {
		// the error happened at the current position;
		// make the error message more specific
		if t.Type == lexer.SEMICOLON && t.Val == "\n" {
			msg += ", found newline"
		} else {
			msg += ", found '" + t.Val + "'"
			//if p.tok.IsLiteral() {
			//	msg += " " + p.lit
			//}
		}
	}
	//p.error(pos, msg)
	fmt.Println(msg)
}

// checkExpr checks that x is an expression (and not a type).
// 检测不是一个类型 而是一个表达式
func (p *TParser) checkExpr(x ast.Expr) ast.Expr {
	switch unparen(x).(type) {
	//case *ast.BadExpr:
	//case *ast.Ident:
	//case *ast.BasicLit:
	//case *ast.FuncLit:
	//case *ast.CompositeLit:
	//case *ast.ParenExpr:
	//	panic("unreachable")
	//case *ast.SelectorExpr:
	case *ast.TIdentExpr:
	//case *ast.SliceExpr:
	//case *ast.TypeAssertExpr:
	// If t.Type == nil we have a type assertion of the form
	// y.(type), which is only allowed in type switch expressions.
	// It's hard to exclude those but for the case where we are in
	// a type switch. Instead be lenient and test this in the type
	// checker.
	//case *ast.CallExpr:
	//case *ast.TStarExpr:
	case *ast.TUnaryExpr:
	case *ast.TBinaryExpr:
	default:
		// all other nodes are not proper expressions
		//p.errorExpected(x.Pos(), "expression")
		//x = &ast.TBadExpr{From: x.Pos(), To: p.safePos(x.End())}
	}
	return x
}

// Returns the CURRENT token if the given type AND value matches.
// Consumes this token on success.
// 匹配确认并返回或报错
func (p *TParser) match(tok int) *lexer.TToken {
	//	pos := p.pos
	t := p.Current()
	if t.Type != tok {
		//p.errorExpected(pos, "'"+tok.String()+"'")
	}
	p.next() // make progress
	return t
}

func (self *TParser) parse_ident() ast.Expr {
	//pos := p.pos
	name := "_"
	token := self.Current()
	if token.Type == lexer.IDENT {
		name = token.Val // p.lit
		self.next()      //	p.next()
	} else {
		// 报告错误
		self.match(lexer.IDENT) // use expect() error handling
	}

	//return &ast.TIdentExpr{
	//	Token: token,
	//	Name:  name}

	return &TVariablePart{
		typ: varTypeIdent,
		s:   name,
	}
}

// #1 解析import
func (self *TParser) __parseImportSpec() {

}

// #2
func (self *TParser) __parseDecl() {

}

// If lhs is set, result list elements which are identifiers are not resolved.
//  a,b:=1,2
func (self *TParser) parse_expr_list(lhs bool) (list []ast.Expr) {
	//if p.trace {
	//	defer un(trace(p, "ExpressionList"))
	//}

	list = append(list, self.checkExpr(self.parse_expr(lhs)))
	t := self.Current()
	for t.Type == lexer.COMMA {
		self.next()
		list = append(list, self.checkExpr(self.parse_expr(lhs)))
	}

	return
}

// 解析表达式左侧Indent
func (self *TParser) parse_lhs_list() []ast.Expr {
	old := self.inRhs
	self.inRhs = false
	list := self.parse_expr_list(true)
	t := self.Current()
	switch t.Type {
	case lexer.DEFINE:
		// lhs of a short variable declaration
		// but doesn't enter scope until later:
		// caller must call p.shortVarDecl(p.makeIdentList(list))
		// at appropriate time.
	case lexer.COLON:
		// lhs of a label declaration or a communication clause of a select
		// statement (parseLhsList is not called when parsing the case clause
		// of a switch statement):
		// - labels are declared by the caller of parseLhsList
		// - for communication clauses, if there is a stand-alone identifier
		//   followed by a colon, we have a syntax error; there is no need
		//   to resolve the identifier in that case
	default:
		// identifiers must be declared elsewhere
		for _, x := range list {
			self.resolve(x)
		}
	}
	self.inRhs = old
	return list
}

func (p *TParser) resolve(x ast.Expr) {
	//p.tryResolve(x, true)
}

// checkExprOrType checks that x is an expression or a type
// (and not a raw type such as [...]T).
//
func (p *TParser) checkExprOrType(x ast.Expr) ast.Expr {
	//switch t := unparen(x).(type) {
	switch unparen(x).(type) {
	case *ast.TParenExpr:
		panic("unreachable")
	case *ast.TUnaryExpr:
	case *ast.TArrayType:
		//if len, isEllipsis := t.Len.(*ast.Ellipsis); isEllipsis {
		//	p.error(len.Pos(), "expected array length, found '...'")
		//	x = &ast.BadExpr{From: x.Pos(), To: p.safePos(x.End())}
		//}
	}

	// all other nodes are expressions or types
	return x
}
func (p *TParser) parse_rhs() ast.Expr {
	old := p.inRhs
	p.inRhs = true
	x := p.checkExpr(p.parse_expr(false))
	p.inRhs = old
	return x
}

func (p *TParser) parse_rhs_or_type() ast.Expr {
	old := p.inRhs
	p.inRhs = true
	x := p.checkExprOrType(p.parse_expr(false))
	p.inRhs = old
	return x
}

// 解析表达式右侧Indent
func (self *TParser) __parseRhsList() []ast.Expr {
	old := self.inRhs
	self.inRhs = true
	list := self.parse_expr_list(false)
	self.inRhs = old
	return list
}

// parseSimpleStmt returns true as 2nd result if it parsed the assignment
// of a range clause (with mode == rangeOk). The returned statement is an
// assignment with a right-hand side that is a single unary expression of
// the form "range x". No guarantees are given for the left-hand side.
func (self *TParser) __parseSimpleStmt(mode int) (ast.Stmt, bool) {
	//x := self.parse_lhs_list()
	return nil, false

}
func (self *TParser) __parseStmt() (s ast.Stmt) {
	/*	if p.trace {
			defer un(trace(p, "Statement"))
		}

		switch p.tok {
		case token.CONST, token.TYPE, token.VAR:
			s = &ast.DeclStmt{Decl: p.parseDecl(syncStmt)}
		case
			// tokens that may start an expression
			token.IDENT, token.INT, token.FLOAT, token.IMAG, token.CHAR, token.STRING, token.FUNC, token.LPAREN, // operands
			token.LBRACK, token.STRUCT, token.MAP, token.CHAN, token.INTERFACE, // composite types
			token.ADD, token.SUB, token.MUL, token.AND, token.XOR, token.ARROW, token.NOT: // unary operators
			s, _ = p.parseSimpleStmt(labelOk)
			// because of the required look-ahead, labeled statements are
			// parsed by parseSimpleStmt - don't expect a semicolon after
			// them
			if _, isLabeledStmt := s.(*ast.LabeledStmt); !isLabeledStmt {
				p.expectSemi()
			}
		case token.GO:
			s = p.parseGoStmt()
		case token.DEFER:
			s = p.parseDeferStmt()
		case token.RETURN:
			s = p.parseReturnStmt()
		case token.BREAK, token.CONTINUE, token.GOTO, token.FALLTHROUGH:
			s = p.parseBranchStmt(p.tok)
		case token.LBRACE:
			s = p.parseBlockStmt()
			p.expectSemi()
		case token.IF:
			s = p.parseIfStmt()
		case token.SWITCH:
			s = p.parseSwitchStmt()
		case token.SELECT:
			s = p.parseSelectStmt()
		case token.FOR:
			s = p.parseForStmt()
		case token.SEMICOLON:
			// Is it ever possible to have an implicit semicolon
			// producing an empty statement in a valid program?
			// (handle correctly anyway)
			s = &ast.EmptyStmt{Semicolon: p.pos, Implicit: p.lit == "\n"}
			p.next()
		case token.RBRACE:
			// a semicolon may be omitted before a closing "}"
			s = &ast.EmptyStmt{Semicolon: p.pos, Implicit: true}
		default:
			// no statement found
			pos := p.pos
			p.errorExpected(pos, "statement")
			syncStmt(p)
			s = &ast.BadStmt{From: pos, To: p.pos}
		}

		return
	*/

	return nil
}

func (p *TParser) parse_selector(v *TVariableResolver) ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "Selector"))
	//}

	sel := p.parse_ident()
	//	return &ast.TSelectorExpr{X: x, Sel: sel}

	v.parts = append(v.parts, sel)
	return v

}

// 当前TOKEN 是逗号
func (p *TParser) at_comma(context string, follow int) bool {
	t := p.Current()
	if t.Type == lexer.COMMA {
		return true
	}
	/*
		if p.tok != follow {
			msg := "missing ','"
			if p.tok == token.SEMICOLON && p.lit == "\n" {
				msg += " before newline"
			}
			p.error(p.pos, msg+" in "+context)
			return true // "insert" comma and continue
		}
	*/
	return false
}

func (p *TParser) parse_value(keyOk bool) ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "Element"))
	//}
	t := p.Current()
	if t.Type == lexer.LBRACE {
		return p.parse_literal_value(nil)
	}

	// Because the parser doesn't know the composite literal type, it cannot
	// know if a key that's an identifier is a struct field name or a name
	// denoting a value. The former is not resolved by the parser or the
	// resolver.
	//
	// Instead, _try_ to resolve such a key if possible. If it resolves,
	// it a) has correctly resolved, or b) incorrectly resolved because
	// the key is a struct field with a name matching another identifier.
	// In the former case we are done, and in the latter case we don't
	// care because the type checker will do a separate field lookup.
	//
	// If the key does not resolve, it a) must be defined at the top
	// level in another file of the same package, the universe scope, or be
	// undeclared; or b) it is a struct field. In the former case, the type
	// checker can do a top-level lookup, and in the latter case it will do
	// a separate field lookup.
	x := p.checkExpr(p.parse_expr(keyOk))
	/*
		if keyOk {
			if p.Current().Type == lexer.COLON {
				// Try to resolve the key but don't collect it
				// as unresolved identifier if it fails so that
				// we don't get (possibly false) errors about
				// undeclared names.
				p.tryResolve(x, false)
			} else {
				// not a key
				p.resolve(x)
			}
		}
	*/
	return x
}
func (p *TParser) parse_element() ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "Element"))
	//}

	x := p.parse_value(true)
	t := p.Current()
	if t.Type == lexer.COLON {
		//colon := p.pos
		p.next()
		x = &ast.TKeyValueExpr{
			Key:   x,
			Colon: t,
			Value: p.parse_value(false)}
	}

	return x
}

// 获取Struct Map 的值{name:fsdf,dfa:fdf}
func (p *TParser) parse_element_list() (list []ast.Expr) {
	//if p.trace {
	//	defer un(trace(p, "ElementList"))
	//}
	t := p.Current()
	for t.Type != lexer.RBRACE && t.Type != lexer.EOF {
		list = append(list, p.parse_element())
		if !p.at_comma("composite literal", lexer.RBRACE) {
			break
		}
		p.next()
	}

	return
}

// xxx[x:x:x]
func (p *TParser) parse_index_or_slice(x ast.Expr) ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "IndexOrSlice"))
	//}

	const N = 3 // change the 3 to 2 to disable 3-index slices
	lbrack := p.match(lexer.LBRACK)
	p.exprLev++
	var index [N]ast.Expr
	var colons [N - 1]*lexer.TToken

	fmt.Println("parse_index_or_slice 0", p.Current(), lbrack.Val)
	if p.Current().Type != lexer.COLON {
		index[0] = p.parse_rhs()
	}
	ncolons := 0
	for p.Current().Type == lexer.COLON && ncolons < len(colons) {
		colons[ncolons] = p.Current()
		ncolons++
		p.next()
		typ := p.Current().Type
		if typ != lexer.COLON && typ != lexer.RBRACK && typ != lexer.EOF {
			index[ncolons] = p.parse_rhs()
		}
	}
	p.exprLev--
	rbrack := p.match(lexer.RBRACK)
	fmt.Println("parse_index_or_slice 1", ncolons, rbrack.Val)
	if ncolons > 0 {
		// slice expression
		slice3 := false
		if ncolons == 2 {
			slice3 = true
			// Check presence of 2nd and 3rd index here rather than during type-checking
			// to prevent erroneous programs from passing through gofmt (was issue 7305).
			if index[1] == nil {
				//p.error(colons[0], "2nd index required in 3-index slice")
				index[1] = &ast.TBadExpr{From: colons[0].Pos + 1, To: colons[1].Pos}
			}
			if index[2] == nil {
				//p.error(colons[1], "3rd index required in 3-index slice")
				index[2] = &ast.TBadExpr{From: colons[1].Pos + 1, To: rbrack.Pos}
			}
		}
		return &ast.TSliceExpr{X: x, Lbrack: lbrack, Low: index[0], High: index[1], Max: index[2], Slice3: slice3, Rbrack: rbrack}
	}

	return &ast.TIndexExpr{X: x, Lbrack: lbrack, Index: index[0], Rbrack: rbrack}

}

// 取值 如Map {}
func (p *TParser) parse_literal_value(x ast.Expr) ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "LiteralValue"))
	//}
	t := p.Current()
	p.match(lexer.LBRACE)
	lbrace := t
	var elts []ast.Expr
	p.exprLev++
	if t.Type != lexer.RBRACE {
		elts = p.parse_element_list()
	}
	p.exprLev--
	p.match(lexer.RBRACE)
	rbrace := p.Current() //p.expectClosing(lexer.RBRACE, "composite literal")
	return &ast.TCompositeLit{
		Type:   x,
		Lbrace: lbrace,
		Elts:   elts,
		Rbrace: rbrace}
}

// 主表达式解析
// If lhs is set and the result is an identifier, it is not resolved.
func (p *TParser) parse_primary_expr(lhs bool) ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "PrimaryExpr"))
	//}

	x := p.parse_operand(lhs) //运算
	// 组合 单个Indent
	if _, ok := x.(*TVariablePart); ok {
		fmt.Println("parse_primary_expr 3", p.Prior())
		resolver := &TVariableResolver{
			locationToken: p.Prior(),
		}
		resolver.parts = append(resolver.parts, x)

		x = resolver
	}
L:
	for {
		t := p.Current()
		fmt.Println("parse_primary_expr 1", t.Val, lexer.TokenNames[t.Type])
		switch t.Type {
		case lexer.PERIOD: //.
			fmt.Println("lexer.PERIOD")
			p.next()
			//if lhs {
			//	p.resolve(x)
			//}

			//resolver := &TVariableResolver{
			//	locationToken: t,
			//}
			//resolver.parts = append(resolver.parts, x)

			switch t.Type {
			case lexer.IDENT: // IDENT.IDENT
				fmt.Println("parse_selector")
				//x = p.parse_selector(p.checkExprOrType(x))
				x = p.parse_selector(x.(*TVariableResolver)) //resolver

			//case lexer.LPAREN:
			//	x = p.parseTypeAssertion(p.checkExpr(x))
			default:
				//pos := p.pos
				pos := p.Current()
				p.errorExpected(pos.Type, "selector or type assertion")
				p.next() // make progress
				sel := &ast.TIdentExpr{Token: pos, Name: "_"}
				x = &ast.TSelectorExpr{X: x, Sel: sel}
			}
			/*
				case lexer.LPAREN:
					if lhs {
						p.resolve(x)
					}
					x = p.parse_call_or_conversion(p.checkExprOrType(x))
			*/
		case lexer.LBRACK:
			//if lhs {
			//	p.resolve(x)
			//}
			x = p.parse_index_or_slice(p.checkExpr(x))

		case lexer.LBRACE: // 解析打括号里的表达式
			if isLiteralType(x) && (p.exprLev >= 0 || !isTypeName(x)) {
				//if lhs {
				//	p.resolve(x)
				//}
				x = p.parse_literal_value(x)
			} else {
				break L
			}

		default:
			//fmt.Println("L", t.Type)
			break L
		}
		lhs = false // no need to try to resolve again
	}

	return x
}

// Unary一元运算
func (self *TParser) parse_unary_expr(lhs bool) ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "UnaryExpr"))
	//}
	t := self.Current()
	switch t.Type {
	case lexer.IDENT, lexer.OPERATOR:
		if t.Type == lexer.IDENT && t.Val != "not" {
			break
		}

		negativeSign := false
		negate := false
		if t.Val == "-" {
			negativeSign = true
		}
		if t.Val == "!" || t.Val == "not" {
			negate = true
		}
		fmt.Println("parse_unary_expr OPERATOR", t)
		//case lexer.ADD, lexer.SUB, lexer.NOT, lexer.XOR, lexer.AND:
		self.next()
		self.skipWhitespace()
		a := self.parse_unary_expr(false)
		return &ast.TUnaryExpr{
			NegativeSign: negativeSign,
			Negate:       negate,
			Operator:     t,
			Term1:        self.checkExpr(a)}

	case lexer.ARROW:
		// channel type or receive expression
		//arrow := p.pos
		self.next()

		// If the next token is token.CHAN we still don't know if it
		// is a channel type or a receive operation - we only know
		// once we have found the end of the unary expression. There
		// are two cases:
		//
		//   <- type  => (<-type) must be channel type
		//   <- expr  => <-(expr) is a receive from an expression
		//
		// In the first case, the arrow must be re-associated with
		// the channel type parsed already:
		//
		//   <- (chan type)    =>  (<-chan type)
		//   <- (chan<- type)  =>  (<-chan (<-type))

		a := self.parse_unary_expr(false)
		/*
			// determine which case we have
			if typ, ok := a.(*ast.ChanType); ok {
				// (<-type)

				// re-associate position info and <-
				dir := ast.SEND
				for ok && dir == ast.SEND {
					if typ.Dir == ast.RECV {
						// error: (<-type) is (<-(<-chan T))
						p.errorExpected(typ.Arrow, "'chan'")
					}
					arrow, typ.Begin, typ.Arrow = typ.Arrow, arrow, arrow
					dir, typ.Dir = typ.Dir, ast.RECV
					typ, ok = typ.Value.(*ast.ChanType)
				}
				if dir == ast.SEND {
					p.errorExpected(arrow, "channel type")
				}

				return x
			}
		*/
		// <-(expr)
		//return &ast.UnaryExpr{OpPos: arrow, Op: token.ARROW, X: p.checkExpr(x)}
		return &ast.TUnaryExpr{
			Operator: t,
			Term1:    self.checkExpr(a)}
	case lexer.MUL:
		// pointer type or unary "*" expression
		/*
			pos := p.pos
			p.next()
			a := self.parseUnaryExpr(false)
			return &ast.StarExpr{Star: pos, X: p.checkExprOrType(x)}
		*/
	}

	return self.parse_primary_expr(lhs)
}

// 获得TOKEN 的优先顺序
func (p *TParser) token_prec() (*lexer.TToken, int) {
	tok := p.Current()
	//t := tok.Type
	if p.inRhs && tok.Type == lexer.ASSIGN {
		//t = lexer.EQL
		tok.Type = lexer.EQL
	}
	//fmt.Println("token_prec", lexer.TokenNames[tok.Type], tok, t, lexer.Precedence(tok))
	return tok, lexer.Precedence(tok)
}

// 解析二元运算
// If lhs is set and the result is an identifier, it is not resolved.
func (self *TParser) parse_binary_expr(lhs bool, prec1 int) ast.Expr {
	a := self.parse_unary_expr(lhs)
	self.skipWhitespace()
	for {
		op, oprec := self.token_prec()
		fmt.Println("parse_binary_expr 0", oprec, op.Val, prec1)
		if oprec < prec1 { // 优先执行
			return a
		}

		fmt.Println("parse_binary_expr 0", self.match(op.Type), op.Val, self.Current())
		self.match(op.Type)
		self.skipWhitespace()
		if lhs {
			self.resolve(a)
			lhs = false
		}

		fmt.Println("parse_binary_expr", lexer.TokenNames[op.Type], op.Val, self.Current())
		b := self.parse_binary_expr(false, oprec+1)
		a = &ast.TBinaryExpr{
			Operator: op,
			Term1:    self.checkExpr(a),
			Term2:    self.checkExpr(b)}
	}
}

func (p *TParser) parse_ternary_expr(lhs bool) ast.Expr {
	// Parse the variable name
	x := p.parse_binary_expr(lhs, lexer.LowestPrec+1)
	//if res_err != nil {
	//	return nil, res_err
	//}

	// Check for identifier
	//if res_eval == nil {
	//	return nil, p.Error("expression must have an identifier at begin.", nil)
	//}

	//	fmt.Println("if TokenKeyword")
	//	# 查找If关键字
	p.skipWhitespace()
	t := p.Current()
	fmt.Println("parse_ternary_expr1", t)
	if t.Val == "if" {
		p.next()
		p.skipWhitespace()
		fmt.Println("parse_ternary_expr", p.Current())
		y := p.parse_binary_expr(lhs, lexer.LowestPrec+1)
		//return nil, p.Error("expression must have an keyword 'if'.", nil)
		// # 创建EvalorEvalor
		node := &TNodeIf{}
		/*
			// #获取IF表达式
			var argsToken []*TToken
			for p.Peek(lexer.IDENT, "else") == nil && p.Remaining() > 1 { //#p.Remaining() > 1 保留1个 }}
				//if p.PeekN(1, TokenSymbol, "}}") != nil {
				//	break
				//}

				// Add token to args
				argsToken = append(argsToken, p.Current())
				p.Consume() // next token

			}

				// # 创建表达式解析器
				argParser := newParser(p.name, argsToken, p.template)
				if len(argsToken) == 0 {
					// This is done to have nice EOF error messages
					argParser.lastToken = tokenName
				}

				if p.template != nil {
					p.template.level++
					defer func() { p.template.level-- }()
				}

				// Parse first and main IF condition
				condition, err := argParser.ParseExpression()
				if err != nil {
					return nil, err
				}
				//		fmt.Println("if condition", condition)
		*/
		node.conditions = append(node.conditions, y) // #条件
		node.wrappers = append(node.wrappers, x)     // #add if value
		p.skipWhitespace()
		fmt.Println("parse_ternary_expr", p.Current())
		if p.Current().Val == "else" {
			p.next()
			p.skipWhitespace()
			z := p.parse_binary_expr(lhs, lexer.LowestPrec+1)
			node.wrappers = append(node.wrappers, z) // #add if value

		}
		//	fmt.Println("if else end", p.Current())
		return node
	}

	return x
}

// 运算
// parseOperand may return an expression or a raw type (incl. array
// types of the form [...]T. Callers must verify the result.
// If lhs is set and the result is an identifier, it is not resolved.
//
func (p *TParser) parse_operand(lhs bool) ast.Expr {
	//if p.trace {
	//	defer un(trace(p, "Operand"))
	//}

	t := p.Current()
	fmt.Println("parse_operand 1", t.Val)
	switch t.Type {
	case lexer.IDENT:
		// 处理Keyword
		//TODO 大小写
		switch t.Val {
		case "true":
			br := &TBoolResolver{
				locationToken: t,
				val:           true,
			}
			p.next()
			return br
		case "false":
			br := &TBoolResolver{
				locationToken: t,
				val:           false,
			}
			p.next()
			return br
		}
		fmt.Println("parse_operand 2", t.Val)
		x := p.parse_ident()
		if !lhs {
			p.resolve(x)
		}

		return x

	case lexer.FLOAT:
		f, err := strconv.ParseFloat(t.Val, 64)
		if err != nil {
			//return nil, p.Error(err.Error(), t)
			goto ERR
		}
		fr := &TFloatResolver{
			locationToken: t,
			val:           f,
		}
		p.next()
		return fr
	case lexer.NUMBER:
		i, err := strconv.Atoi(t.Val)
		if err != nil {
			//return nil, p.Error(err.Error(), t)
			goto ERR
		}
		nr := &TIntResolver{
			locationToken: t,
			val:           i,
		}
		p.next()
		return nr
	case lexer.STRING:
		//case lexer.INT, lexer.FLOAT, lexer.IMAG, lexer.CHAR, lexer.STRING:
		/*x := &ast.TBasicLit{
			Kind:  t.Type,
			Value: t.Val}

		p.next()
		return x*/
		sr := &TStringResolver{
			locationToken: t,
			val:           t.Val,
		}
		p.next()
		return sr
	case lexer.KEYWORD:

	case lexer.LPAREN:
		fmt.Println("parse_operand 3", p.Current().Val)
		lparen := p.Current()
		p.next()
		p.exprLev++
		x := p.parse_rhs_or_type() // types may be parenthesized: (some type)
		p.exprLev--
		rparen := p.match(lexer.RPAREN)
		fmt.Println("parse_operand 4", p.Current().Val)
		return &ast.TParenExpr{Lparen: lparen, Term1: x, Rparen: rparen}

	case lexer.LBRACE: // map   {key:val}
		//case token.FUNC:
		//	return p.parseFuncTypeOrLit()

	}
	/*
		if typ := p.tryIdentOrType(); typ != nil {
			// could be type for composite literal or conversion
			_, isIdent := typ.(*ast.Ident)
			assert(!isIdent, "type cannot be identifier")
			return typ
		}
	*/
ERR:
	// we have an error
	pos := p.pos
	//p.errorExpected(pos, "operand")
	//syncStmt(p)
	return &ast.TBadExpr{
		From: pos,
		To:   p.pos}
}

func (self *TParser) parse_expr(lhs bool) ast.Expr {
	// 解析3元运算
	return self.parse_ternary_expr(lhs)
}

func (self *TParser) Parse(expr string) ast.Expr {
	lex, err := lexer.NewLexer(strings.NewReader(expr))
	if err != nil {

	}

	self.lexer = lex
	for {
		token, ok := <-lex.Tokens
		if !ok {
			break
		}

		self.tokens = append(self.tokens, &token)
		fmt.Println(lexer.PrintToken(token))
	}
	//fmt.Println("complete")

	//parser.Count = len(parser.items)
	return self.parse_expr(false)
}

package ast

import (
	"eval/context"
	"lexer"
)

// 基本数据类型

type (
	// An ArrayType node represents an array or slice type.
	TArrayType struct {
		Lbrack lexer.TToken // position of "["
		Len    Expr         // Ellipsis node for [...]T array types, nil for slice types
		Elt    Expr         // element type
	}

	// A MapType node represents a map type.
	TMapType struct {
		Token lexer.TToken // position of "{" keyword
		Pairs map[Expr]Expr
	}
)

func (self *TArrayType) exprNode() {}
func (expr *TArrayType) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	return nil, nil
}

func (self *TMapType) exprNode() {}
func (expr *TMapType) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	return nil, nil
}

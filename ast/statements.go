package ast

import (
	"vectors/lexer"
)

type (
	// A BranchStmt node represents a break, continue, goto,
	// or fallthrough statement.
	//
	BranchStmt struct {
		//TokPos token.Pos   // position of Tok
		//Tok    token.Token // keyword token (BREAK, CONTINUE, GOTO, FALLTHROUGH)
		//Label      *Ident // label name; or nil
		Token      lexer.TToken
		Statements []Stmt
	}

	// A BlockStmt node represents a braced statement list.
	BlockStmt struct {
		//Lbrace     token.Pos // position of "{"
		//List       []Stmt
		//Rbrace     token.Pos // position of "}"
		Token      lexer.TToken
		Statements []Stmt
	}
)

func (*BranchStmt) stmtNode() {}
func (*BlockStmt) stmtNode()  {}

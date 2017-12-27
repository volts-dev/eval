package context

import (
	"lexer"
)

// ExecutionContext contains all data important for the current rendering state.
//
// If you're writing a custom tag, your tag's Execute()-function will
// have access to the ExecutionContext. This struct stores anything
// about the current rendering process's Context including
// the Context provided by the user (field Public).
// You can safely use the Private context to provide data to the user's
// template (like a 'forloop'-information). The Shared-context is used
// to share data between tags. All ExecutionContexts share this context.
//
// Please be careful when accessing the Public data.
// PLEASE DO NOT MODIFY THE PUBLIC CONTEXT (read-only).
//
// To create your own execution context within tags, use the
// NewChildExecutionContext(parent) function.
type ExecutionContext struct {
	Autoescape bool
	Public     TContext // 外部使用者传进来的变量
	Private    TContext // 内部创作者定义的变量 如版本
	Shared     TContext
}

func newExecutionContext(ctx TContext) *ExecutionContext {
	privateCtx := make(TContext)

	// Make the pongo2-related funcs/vars available to the context
	//privateCtx["pongo2"] = pongo2MetaContext

	return &ExecutionContext{
		//template: tpl,

		Public:     ctx,
		Private:    privateCtx,
		Autoescape: true,
	}
}

func NewChildExecutionContext(parent *ExecutionContext) *ExecutionContext {
	newctx := &ExecutionContext{
		//template: parent.template,

		Public:     parent.Public,
		Private:    make(TContext),
		Autoescape: parent.Autoescape,
	}
	newctx.Shared = parent.Shared

	// Copy all existing private items
	newctx.Private.Update(parent.Private)

	return newctx
}

func (ctx *ExecutionContext) Error(msg string, token *lexer.TToken) *lexer.Error {
	//filename := ctx.template.Name
	filename := ""
	var line, col int
	if token != nil {
		// No tokens available
		// TODO: Add location (from where?)
		filename = "" //token.Filename
		line = token.Line
		col = token.Col
	}
	return &lexer.Error{
		//	Template: ctx.template,
		Filename: filename,
		Line:     line,
		Column:   col,
		Token:    token,
		Sender:   "execution",
		ErrorMsg: msg,
	}
}

func (ctx *ExecutionContext) Logf(format string, args ...interface{}) {
	//	ctx.template.set.logf(format, args...)
}

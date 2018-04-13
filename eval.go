package eval

import (
	"fmt"
	"vectors/eval/ast"
	"vectors/eval/context"
	"vectors/eval/parser"
)

func Eval(expr string, ctx ...map[string]interface{}) (*ast.Value, *ast.Error) {
	parser := parser.NewParser()

	x := parser.Parse(expr)
	fmt.Println("-----Parse------------------------------------------")
	privateCtx := make(context.TContext)
	// Make the pongo2-related funcs/vars available to the context
	privateCtx["ver"] = "0.01"

	c := &context.ExecutionContext{
		//template: tpl,
		//Public:     ctx[0],
		Private:    privateCtx,
		Autoescape: true,
	}

	if len(ctx) > 0 {
		c.Public = context.TContext(ctx[0])
		return x.Execute(c)
	}

	return x.Execute(c)
}

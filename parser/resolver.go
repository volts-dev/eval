package parser

import (
	"fmt"
	"reflect"
	"strconv"
	"strings"

	"github.com/VectorsOrigin/eval/ast"
	"github.com/VectorsOrigin/eval/context"
	"github.com/VectorsOrigin/lexer"
)

const (
	varTypeInt = iota
	varTypeIdent
)

type (
	// Indet变量
	TVariablePart struct {
		typ int
		s   string
		i   int

		isFunctionCall bool
		callingArgs    []TFunctionCallArgument // needed for a function call, represents all argument nodes (INode supports nested function calls)
	}

	TFunctionCallArgument interface {
		Execute(*context.ExecutionContext) (*ast.Value, *ast.Error)
	}

	// TODO: Add location tokens
	TStringResolver struct {
		locationToken *lexer.TToken
		val           string
	}

	TIntResolver struct {
		locationToken *lexer.TToken
		val           int
	}

	TFloatResolver struct {
		locationToken *lexer.TToken
		val           float64
	}

	TBoolResolver struct {
		locationToken *lexer.TToken
		val           bool
	}

	TMapResolver struct {
		locationToken *lexer.TToken
		val           bool
	}

	// 变量选择器
	TVariableResolver struct {
		locationToken *lexer.TToken

		parts []ast.Expr // []*variablePart
	}

	TNodeFilteredVariable struct {
		locationToken *lexer.TToken

		//		resolver    IEvaluator
		//		filterChain []*filterCall
	}

	TNodeIf struct {
		locationToken *lexer.TToken
		conditions    []ast.Expr
		wrappers      []ast.Expr
	}
)

/*
func (v *nodeFilteredVariable) Execute(ctx *context.ExecutionContext, writer TemplateWriter) *ast.Error {
	value, err := v.Evaluate(ctx)
	if err != nil {
		return err
	}
	writer.WriteString(value.String())
	return nil
}

func (vr *variableResolver) Execute(ctx *context.ExecutionContext, writer TemplateWriter) *ast.Error {
	value, err := vr.Evaluate(ctx)
	if err != nil {
		return err
	}
	writer.WriteString(value.String())
	return nil
}

func (s *stringResolver) Execute(ctx *context.ExecutionContext, writer TemplateWriter) *ast.Error {
	value, err := s.Evaluate(ctx)
	if err != nil {
		return err
	}
	writer.WriteString(value.String())
	return nil
}

func (i *intResolver) Execute(ctx *context.ExecutionContext, writer TemplateWriter) *ast.Error {
	value, err := i.Evaluate(ctx)
	if err != nil {
		return err
	}
	writer.WriteString(value.String())
	return nil
}

func (f *floatResolver) Execute(ctx *context.ExecutionContext, writer TemplateWriter) *ast.Error {
	value, err := f.Evaluate(ctx)
	if err != nil {
		return err
	}
	writer.WriteString(value.String())
	return nil
}

func (b *boolResolver) Execute(ctx *context.ExecutionContext, writer TemplateWriter) *ast.Error {
	value, err := b.Evaluate(ctx)
	if err != nil {
		return err
	}
	writer.WriteString(value.String())
	return nil
}

func (v *nodeFilteredVariable) GetPositionToken() *lexer.TToken {
	return v.locationToken
}

func (vr *variableResolver) GetPositionToken() *lexer.TToken {
	return vr.locationToken
}

func (s *stringResolver) GetPositionToken() *lexer.TToken {
	return s.locationToken
}

func (i *intResolver) GetPositionToken() *lexer.TToken {
	return i.locationToken
}

func (f *floatResolver) GetPositionToken() *lexer.TToken {
	return f.locationToken
}

func (b *boolResolver) GetPositionToken() *lexer.TToken {
	return b.locationToken
}
*/
func (self *TStringResolver) exprNode() {}
func (s *TStringResolver) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	return ast.AsValue(s.val), nil
}
func (self *TIntResolver) exprNode() {}
func (i *TIntResolver) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	return ast.AsValue(i.val), nil
}

func (self *TFloatResolver) exprNode() {}
func (f *TFloatResolver) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	return ast.AsValue(f.val), nil
}

func (self *TBoolResolver) exprNode() {}
func (b *TBoolResolver) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	return ast.AsValue(b.val), nil
}

func (s *TStringResolver) FilterApplied(name string) bool {
	return false
}

func (i *TIntResolver) FilterApplied(name string) bool {
	return false
}

func (f *TFloatResolver) FilterApplied(name string) bool {
	return false
}

func (b *TBoolResolver) FilterApplied(name string) bool {
	return false
}

func (vr *TVariableResolver) FilterApplied(name string) bool {
	return false
}

func (vr *TVariableResolver) String() string {
	parts := make([]string, 0, len(vr.parts))
	for _, p := range vr.parts {
		//TODO 添加接口Type接口到表达式
		v := p.(*TVariablePart)
		switch v.typ {
		case varTypeInt:
			parts = append(parts, strconv.Itoa(v.i))
		case varTypeIdent:
			parts = append(parts, v.s)
		default:
			panic("unimplemented")
		}
	}
	return strings.Join(parts, ".")
}

func (vr *TVariableResolver) resolve(ctx *context.ExecutionContext) (*ast.Value, error) {
	var current reflect.Value
	var isSafe bool

	//fmt.Println("resolve1", ctx)
	for idx, part := range vr.parts {
		indent := part.(*TVariablePart)
		if idx == 0 {
			// We're looking up the first part of the variable.
			// First we're having a look in our private
			// context (e. g. information provided by tags, like the forloop)
			fmt.Println("resolve 2", indent.s, ctx.Public[indent.s])
			val, inPrivate := ctx.Private[indent.s]
			if !inPrivate {
				// Nothing found? Then have a final lookup in the public context
				val = ctx.Public[indent.s]
			}

			// Raw 字符串
			//if val == nil {
			//	//val = indent.s
			//}

			if isKeyword(indent.s) {
				val = indent.s
			}
			current = reflect.ValueOf(val) // Get the initial value
		} else {
			// Next parts, resolve it from current

			// Before resolving the pointer, let's see if we have a method to call
			// Problem with resolving the pointer is we're changing the receiver
			isFunc := false
			if indent.typ == varTypeIdent {
				funcValue := current.MethodByName(indent.s)
				if funcValue.IsValid() {
					current = funcValue
					isFunc = true
				}
			}

			if !isFunc {
				// If current a pointer, resolve it
				if current.Kind() == reflect.Ptr {
					current = current.Elem()
					if !current.IsValid() {
						// Value is not valid (anymore)
						return ast.AsValue(nil), nil
					}
				}

				// Look up which part must be called now
				switch indent.typ {
				case varTypeInt:
					// Calling an index is only possible for:
					// * slices/arrays/strings
					switch current.Kind() {
					case reflect.String, reflect.Array, reflect.Slice:
						if indent.i >= 0 && current.Len() > indent.i {
							current = current.Index(indent.i)
						} else {
							// Access to non-existed index will cause an error
							return nil, fmt.Errorf("%s is out of range (type: %s, length: %d)",
								vr.String(), current.Kind().String(), current.Len())
						}
					default:
						return nil, fmt.Errorf("Can't access an index on type %s (variable %s)",
							current.Kind().String(), vr.String())
					}
				case varTypeIdent:
					// debugging:
					// fmt.Printf("now = %s (kind: %s)\n", part.s, current.Kind().String())

					// Calling a field or key
					switch current.Kind() {
					case reflect.Struct:
						current = current.FieldByName(indent.s)
					case reflect.Map:
						current = current.MapIndex(reflect.ValueOf(indent.s))
					default:
						return nil, fmt.Errorf("Can't access a field by name on type %s (variable %s)",
							current.Kind().String(), vr.String())
					}
				default:
					panic("unimplemented")
				}
			}
		}

		if !current.IsValid() {
			// Value is not valid (anymore)
			return ast.AsValue(nil), nil
		}

		// If current is a reflect.ValueOf(pongo2.Value), then unpack it
		// Happens in function calls (as a return value) or by injecting
		// into the execution context (e.g. in a for-loop)
		if current.Type() == reflect.TypeOf(&ast.Value{}) {
			tmpValue := current.Interface().(*ast.Value)
			current = tmpValue.Val
			isSafe = tmpValue.Safe
		}

		// Check whether this is an interface and resolve it where required
		if current.Kind() == reflect.Interface {
			current = reflect.ValueOf(current.Interface())
		}

		// Check if the part is a function call
		if indent.isFunctionCall || current.Kind() == reflect.Func {
			// Check for callable
			if current.Kind() != reflect.Func {
				return nil, fmt.Errorf("'%s' is not a function (it is %s)", vr.String(), current.Kind().String())
			}

			// Check for correct function syntax and types
			// func(*Value, ...) *Value
			t := current.Type()

			// Input arguments
			if len(indent.callingArgs) != t.NumIn() && !(len(indent.callingArgs) >= t.NumIn()-1 && t.IsVariadic()) {
				return nil,
					fmt.Errorf("Function input argument count (%d) of '%s' must be equal to the calling argument count (%d).",
						t.NumIn(), vr.String(), len(indent.callingArgs))
			}

			// Output arguments
			if t.NumOut() != 1 {
				return nil, fmt.Errorf("'%s' must have exactly 1 output argument", vr.String())
			}

			// Evaluate all parameters
			var parameters []reflect.Value

			numArgs := t.NumIn()
			isVariadic := t.IsVariadic()
			var fnArg reflect.Type

			for idx, arg := range indent.callingArgs {
				pv, err := arg.Execute(ctx)
				if err != nil {
					return nil, nil //err
				}

				if isVariadic {
					if idx >= t.NumIn()-1 {
						fnArg = t.In(numArgs - 1).Elem()
					} else {
						fnArg = t.In(idx)
					}
				} else {
					fnArg = t.In(idx)
				}

				if fnArg != reflect.TypeOf(new(ast.Value)) {
					// Function's argument is not a *pongo2.Value, then we have to check whether input argument is of the same type as the function's argument
					if !isVariadic {
						if fnArg != reflect.TypeOf(pv.Interface()) && fnArg.Kind() != reflect.Interface {
							return nil, fmt.Errorf("Function input argument %d of '%s' must be of type %s or *pongo2.Value (not %T).",
								idx, vr.String(), fnArg.String(), pv.Interface())
						}
						// Function's argument has another type, using the interface-value
						parameters = append(parameters, reflect.ValueOf(pv.Interface()))
					} else {
						if fnArg != reflect.TypeOf(pv.Interface()) && fnArg.Kind() != reflect.Interface {
							return nil, fmt.Errorf("Function variadic input argument of '%s' must be of type %s or *pongo2.Value (not %T).",
								vr.String(), fnArg.String(), pv.Interface())
						}
						// Function's argument has another type, using the interface-value
						parameters = append(parameters, reflect.ValueOf(pv.Interface()))
					}
				} else {
					// Function's argument is a *pongo2.Value
					parameters = append(parameters, reflect.ValueOf(pv))
				}
			}

			// Check if any of the values are invalid
			for _, p := range parameters {
				if p.Kind() == reflect.Invalid {
					return nil, fmt.Errorf("Calling a function using an invalid parameter")
				}
			}

			// Call it and get first return parameter back
			rv := current.Call(parameters)[0]

			if rv.Type() != reflect.TypeOf(new(ast.Value)) {
				current = reflect.ValueOf(rv.Interface())
			} else {
				// Return the function call value
				current = rv.Interface().(*ast.Value).Val
				isSafe = rv.Interface().(*ast.Value).Safe
			}
		}

		if !current.IsValid() {
			// Value is not valid (e. g. NIL value)
			return ast.AsValue(nil), nil
		}
	}

	return &ast.Value{Val: current, Safe: isSafe}, nil
}

func (self *TVariablePart) ExprNode() {}
func (vr *TVariablePart) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	return nil, nil
}

func (self *TVariableResolver) exprNode() {}
func (vr *TVariableResolver) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	value, err := vr.resolve(ctx)
	if err != nil {
		return ast.AsValue(nil), nil //ctx.Error(err.Error(), vr.locationToken)
	}
	return value, nil
}

func (v *TNodeFilteredVariable) FilterApplied(name string) bool {
	/*	for _, filter := range v.filterChain {
		if filter.name == name {
			return true
		}
	}*/
	return false
}

func (self *TNodeFilteredVariable) exprNode() {}
func (v *TNodeFilteredVariable) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	/*	value, err := v.resolver.Evaluate(ctx)
		if err != nil {
			return nil, err
		}

		for _, filter := range v.filterChain {
			value, err = filter.Execute(value, ctx)
			if err != nil {
				return nil, err
			}
		}

		return value, nil*/

	return nil, nil
}

func (self *TNodeIf) exprNode() {}

// var if expr else not
func (node *TNodeIf) Execute(ctx *context.ExecutionContext) (*ast.Value, *ast.Error) {
	fmt.Println("Evaluate node1", len(node.conditions), len(node.wrappers))
	for i, condition := range node.conditions {
		fmt.Println("Evaluate node", condition)
		result, err := condition.Execute(ctx)
		if err != nil {
			return nil, err
		}
		fmt.Println("Evaluate node3", result.IsNil(), result.IsBool(), result.IsString())
		if result.IsTrue() {
			return node.wrappers[i].Execute(ctx)
		}

		// Last value
		if len(node.wrappers) > i+1 {
			fmt.Println("Evaluate node4", node.wrappers[i+1])
			return node.wrappers[i+1].Execute(ctx)
		} else {
			return ast.AsValue(nil), nil
		}

	}

	return nil, nil
}

/*
func (node *nodeIf) Execute(ctx *context.ExecutionContext, writer TemplateWriter) *ast.Error {
	for i, condition := range node.conditions {
		result, err := condition.Execute(ctx)
		if err != nil {
			return err
		}

		if result.IsTrue() {
			return node.wrappers[i].Execute(ctx, writer)
		}

		// Last value
		if len(node.wrappers) > i+1 {
			return node.wrappers[i+1].Execute(ctx, writer)
		}

	}

	return nil
}
*/

func (s *TNodeIf) FilterApplied(name string) bool {
	return false
}

func (v *TNodeIf) GetPositionToken() *lexer.TToken {
	return v.locationToken
}

package ast

import (
	"fmt"
	"math"
	"reflect"

	"github.com/volts-dev/eval/context"
	"github.com/volts-dev/lexer"
)

type (
	// A BadExpr node is a placeholder for expressions containing
	// syntax errors for which no correct expression nodes can be
	// created.
	//
	TBadExpr struct {
		From, To int // position range of bad expression
	}
	/*
		// A BasicLit node represents a literal of basic type.
		TBasicLit struct {
			//ValuePos token.Pos   // literal position
			Kind  int    // token.INT, token.FLOAT, token.IMAG, token.CHAR, or token.STRING
			Value string // literal string; e.g. 42, 0x7f, 3.14, 1e-9, 2.4i, 'a', '\x7f', "foo" or `\m\n\o`
		}


		// A FuncLit node represents a function literal.
		FuncLit struct {
			Type *FuncType  // function type
			Body *BlockStmt // function body
		}
	*/

	// A KeyValueExpr node represents (key : value) pairs
	// in composite literals.
	//
	TKeyValueExpr struct {
		Key   Expr
		Colon *lexer.TToken // position of ":"
		Value Expr
	}

	// map slice struct集合
	// A CompositeLit node represents a composite literal.
	TCompositeLit struct {
		Type   Expr          // literal type; or nil
		Lbrace *lexer.TToken // position of "{"
		Elts   []Expr        // list of composite elements; or nil
		Rbrace *lexer.TToken // position of "}"
	}

	// An Ident node represents an identifier.
	TIdentExpr struct {
		//NamePos token.Pos // identifier position
		//Name    string    // identifier name
		//Obj     *Object   // denoted object; or nil
		Token *lexer.TToken
		Name  string
	}

	// An IndexExpr node represents an expression followed by an index.
	TIndexExpr struct {
		X      Expr          // expression
		Lbrack *lexer.TToken // position of "["
		Index  Expr          // index expression
		Rbrack *lexer.TToken // position of "]"
	}

	// An SliceExpr node represents an expression followed by slice indices.
	TSliceExpr struct {
		X      Expr          // expression
		Lbrack *lexer.TToken // position of "["
		Low    Expr          // begin of slice range; or nil
		High   Expr          // end of slice range; or nil
		Max    Expr          // maximum capacity of slice; or nil
		Slice3 bool          // true if 3-index slice (2 colons present)
		Rbrack *lexer.TToken // position of "]"
	}

	// A SelectorExpr node represents an expression followed by a selector.
	TSelectorExpr struct {
		X   Expr        // expression
		Sel *TIdentExpr // field selector
	}

	// A UnaryExpr node represents a unary expression.
	// Unary "*" expressions are represented via StarExpr nodes.
	// 一 元
	TUnaryExpr struct {
		//OpPos token.Pos   // position of Op
		//Op    token.Token // operator
		//X     Expr        // operand
		Negate       bool // 反表达式
		NegativeSign bool // --
		Term1        Expr
		Operator     *lexer.TToken // operator
	}

	// A BinaryExpr node represents a binary expression.
	// 二元
	TBinaryExpr struct {
		//X     Expr         // left operand
		//OpPos token.Pos    // position of Op
		//Op    lexer.TToken // operator
		//Y     Expr         // right operand
		negate       bool
		negativeSign bool
		Term1        Expr
		Term2        Expr
		Operator     *lexer.TToken
	}

	// A ParenExpr node represents a parenthesized expression.
	// 括号运算
	TParenExpr struct {
		Lparen *lexer.TToken //*lexer.TToken // position of "("
		Term1  Expr          // parenthesized expression
		Rparen *lexer.TToken //*lexer.TToken // position of ")"
	}
)

func (self *TKeyValueExpr) exprNode() {}
func (expr *TKeyValueExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	return nil, nil
}

func (self *TCompositeLit) exprNode() {}

// 见exprInternal
func (self *TCompositeLit) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	/*var typ, base Expr

	switch  {
	case self.Type != nil:
		// composite literal type present - use it
		// [...]T array types may only appear with composite literals.
		// Check for them here so we don't have to handle ... in general.
		if atyp, _ := self.Type.(*ast.ArrayType); atyp != nil && atyp.Len != nil {
			if ellip, _ := atyp.Len.(*ast.Ellipsis); ellip != nil && ellip.Elt == nil {
				// We have an "open" [...]T array type.
				// Create a new ArrayType with unknown length (-1)
				// and finish setting it up after analyzing the literal.
				typ = &Array{len: -1, elem: check.typ(atyp.Elt)}
				base = typ
				break
			}
		}
		typ = check.typ(e.Type)
		base = typ

	case hint != nil:
		// no composite literal type present - use hint (element type of enclosing type)
		//typ = hint
		//base, _ = deref(typ.Underlying()) // *T implies &T{}

	default:
		// TODO(gri) provide better error messages depending on context
		//check.error(e.Pos(), "missing type in composite literal")
		//goto Error
	}
	*/
	switch self.Type.(type) {
	default:

		// Prevent crash if the map referred to is not yet set up.
		// See analogous comment for *Array.
		//if utyp.key == nil || utyp.elem == nil {
		//	check.error(e.Pos(), "illegal cycle in type declaration")
		//	goto Error
		//}
		visited := make(map[string]interface{}, len(self.Elts))
		for _, e := range self.Elts {
			kv, _ := e.(*TKeyValueExpr)
			if kv == nil {
				//check.error(e.Pos(), "missing key in map literal")
				continue
			}
			k, _ := kv.Key.Execute(ctx)
			if k == nil {
				continue // TODO 返回错误
			}

			v, _ := kv.Value.Execute(ctx)
			if v == nil {
				continue // TODO 返回错误
			}
			fmt.Println("TCompositeLit) Execute", k.String(), v.Interface())
			visited[k.String()] = v.Interface()
		}

		return AsValue(visited), nil
	}

	return nil, nil
}

func (self *TIdentExpr) exprNode() {}
func (self *TIdentExpr) String() string {
	if self != nil {
		return self.Name
	}
	return "<nil>"
}
func (expr *TIdentExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	return nil, nil
}

func (self *TSelectorExpr) exprNode() {}
func (expr *TSelectorExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	return nil, nil
}

func (self *TIndexExpr) exprNode() {}
func (expr *TIndexExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	idx, err := expr.Index.Execute(ctx)
	if err != nil {

	}

	iden, err := expr.X.Execute(ctx)
	if err != nil {

	}
	//fmt.Println("TIndexExpr) Execute 0", iden, idx)
	if idx != nil && idx.Integer() <= iden.Len() {
		//fmt.Println("TIndexExpr) Execute 1", iden.getResolvedValue().Index(idx.Integer()).Kind(), iden.Len(), iden.getResolvedValue().Kind(), idx.Integer(), iden.getResolvedValue().Index(idx.Integer()))
		val := iden.getResolvedValue().Index(idx.Integer())
		switch val.Kind() {
		case reflect.Bool:
			return AsValue(val.Bool()), nil
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			return AsValue(val.Int()), nil
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			return AsValue(val.Uint()), nil
		case reflect.Float32, reflect.Float64:
			return AsValue(val.Float()), nil
		case reflect.String: // for []string{"1","2"}
			return AsValue(val.String()), nil
		case reflect.Interface: // for []interface{}{1,"2"}
			//fmt.Println("TIndexExpr) Execute 12", val.Interface())
			return AsValue(val.Interface()), nil
		default:
			panic("not possible")
		}
	}
	return AsValue(nil), nil
}

func (self *TSliceExpr) exprNode() {}
func (expr *TSliceExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	//TODO 实现TSliceExpr.Execute
	return nil, nil
}

func (self *TBadExpr) exprNode() {}
func (expr *TBadExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	return nil, nil
}

func (self *TBinaryExpr) exprNode() {}
func (expr *TBinaryExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	t1, err := expr.Term1.Execute(ctx)
	if err != nil {
		return nil, err
	}
	result := t1

	if expr.negate {
		result = result.Negate()
	}

	if expr.negativeSign {
		if result.IsNumber() {
			switch {
			case result.IsFloat():
				result = AsValue(-1 * result.Float())
			case result.IsInteger():
				result = AsValue(-1 * result.Integer())
			default:
				panic("not possible")
			}
		} else {
			//TODO
			return nil, nil //ctx.Error("Negative sign on a non-number expression", nil) //, expr.GetPositionToken())
		}
	}
	//fmt.Println("simpleExpression", t1)
	if expr.Term2 != nil {
		t2, err := expr.Term2.Execute(ctx)
		if err != nil {
			return nil, err
		}

		// TODO del
		defer fmt.Println("TBinaryExpr.Execute:", t1, expr.Operator.Val, t2)

		//fmt.Println("TBinaryExpr.Execute:", expr.Operator, expr.Operator.Val)
		switch expr.Operator.Val {
		case "in":
			//fmt.Println("TBinaryExpr.Execute t2.Contains(t1):", t2.Contains(t1))
			return AsValue(t2.Contains(t1)), nil
		case "and", "&&":
			if t1 == nil || t2 == nil {
				return AsValue(false), nil
			}

			//fmt.Println("AND", t1.Interface(), t2.Interface())
			/*if t1.IsBool() && !t2.IsBool() {
				if t1.IsTrue() {
					return AsValue(t2.Interface()), nil
				}
				return AsValue(t1.IsTrue()), nil
			}*/

			return AsValue(t1.IsTrue() && t2.IsTrue()), nil

		case "or", "||": // # 新添加以区分返回值还是布尔值
			// # 当都是布尔类型时返回布尔类型
			if (t1.IsBool()) && t2.IsBool() {
				return AsValue(t1.IsTrue() || t2.IsTrue()), nil
			}

			//fmt.Println("OR", t1.IsBool(), t2.IsBool(), expr.operator.Val, t1.Interface(), t2.Interface())
			if t1.IsNil() {
				if t2.IsNil() { //# 当V1和V2都是Nil时返回false
					return AsValue(false), nil
				}
				// # 否则返回V2
				return AsValue(t2.Interface()), nil

			} else {
				if t1.IsBool() && !t1.IsTrue() {
					return AsValue(t2.Interface()), nil
				}

				return AsValue(t1.Interface()), nil
			}
		case "+":
			//fmt.Println("TBinaryExpr.Execute:", result, t2)
			if result.IsFloat() || t2.IsFloat() {
				// Result will be a float
				fmt.Println("TBinaryExpr.Execute:", -531441000000.000000+-36, result.Float(), t2.Float())
				return AsValue(result.Float() + t2.Float()), nil
			}
			// Result will be an integer
			return AsValue(result.Integer() + t2.Integer()), nil
		case "-":
			if result.IsFloat() || t2.IsFloat() {
				// Result will be a float
				return AsValue(result.Float() - t2.Float()), nil
			}
			// Result will be an integer
			return AsValue(result.Integer() - t2.Integer()), nil

		case "*":
			if t1.IsFloat() || t2.IsFloat() {
				// Result will be float
				return AsValue(t1.Float() * t2.Float()), nil
			}
			// Result will be int
			return AsValue(t1.Integer() * t2.Integer()), nil
		case "/":
			if t1.IsFloat() || t2.IsFloat() {
				// Result will be float
				return AsValue(t1.Float() / t2.Float()), nil
			}
			// Result will be int
			return AsValue(t1.Integer() / t2.Integer()), nil
		case "%":
			// Result will be int
			return AsValue(t1.Integer() % t2.Integer()), nil

		case "^":
			return AsValue(math.Pow(t1.Float(), t2.Float())), nil
		case "<=":
			if t1.IsFloat() || t2.IsFloat() {
				return AsValue(t1.Float() <= t2.Float()), nil
			}
			return AsValue(t1.Integer() <= t2.Integer()), nil
		case ">=":
			if t1.IsFloat() || t2.IsFloat() {
				return AsValue(t1.Float() >= t2.Float()), nil
			}
			return AsValue(t1.Integer() >= t2.Integer()), nil
		case "==":
			return AsValue(t1.EqualValueTo(t2)), nil
		case ">":
			if t1.IsFloat() || t2.IsFloat() {
				return AsValue(t1.Float() > t2.Float()), nil
			}
			return AsValue(t1.Integer() > t2.Integer()), nil
		case "<":
			if t1.IsFloat() || t2.IsFloat() {
				return AsValue(t1.Float() < t2.Float()), nil
			}
			return AsValue(t1.Integer() < t2.Integer()), nil
		case "!=", "<>":
			fmt.Println("!=", t1.EqualValueTo(t2))
			return AsValue(!t1.EqualValueTo(t2)), nil

		default:
			panic("unimplemented")
		}
	}

	return result, nil
}

func (self *TParenExpr) exprNode() {}
func (expr *TParenExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	t1, err := expr.Term1.Execute(ctx)
	if err != nil {
		return nil, err
	}

	return t1, nil
}

func (self *TUnaryExpr) exprNode() {}
func (expr *TUnaryExpr) Execute(ctx *context.ExecutionContext) (*Value, *Error) {
	result, err := expr.Term1.Execute(ctx)
	if err != nil {
		return nil, err
	}

	//fmt.Println("TUnaryExpr,0", expr.NegativeSign, result)
	if expr.Negate {
		result = result.Negate()
	}

	defer fmt.Println("TUnaryExpr.Execute:", expr.Term1, result)
	if expr.NegativeSign {
		if result.IsNumber() {
			switch {
			case result.IsFloat():
				result = AsValue(-1 * result.Float())
			case result.IsInteger():
				result = AsValue(-1 * result.Integer())
			default:
				panic("not possible")
			}
		} else {
			//TODO
			return nil, nil // ctx.Error("Negative sign on a non-number expression", nil) //, expr.GetPositionToken())
		}
	}

	return result, nil
}

package context

import (
	"fmt"
	"lexer"
	"regexp"
)

var reIdentifiers = regexp.MustCompile("^[a-zA-Z0-9_]+$")

// A Context type provides constants, variables, instances or functions to a template.
//
// pongo2 automatically provides meta-information or functions through the "pongo2"-key.
// Currently, context["pongo2"] contains the following keys:
//  1. version: returns the version string
//
// Template examples for accessing items from your context:
//     {{ myconstant }}
//     {{ myfunc("test", 42) }}
//     {{ user.name }}
//     {{ pongo2.version }}
type TContext map[string]interface{}

func NewContext() TContext {
	return make(TContext)
}
func (self TContext) checkForValidIdentifiers() *lexer.Error {
	for k, v := range self {
		if !reIdentifiers.MatchString(k) {
			return &lexer.Error{
				Sender:   "checkForValidIdentifiers",
				ErrorMsg: fmt.Sprintf("Context-key '%s' (value: '%+v') is not a valid identifier.", k, v),
			}
		}
	}
	return nil
}

// Update updates this context with the key/value-pairs from another context.
func (self TContext) Update(other TContext) TContext {
	for k, v := range other {
		self[k] = v
	}
	return self
}

func (self TContext) Copy() TContext {
	ctx := TContext{}

	for key, val := range self {
		ctx[key] = val
	}

	return ctx
}

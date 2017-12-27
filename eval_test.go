package eval

import (
	"testing"
)

func TestMain(t *testing.T) {
	m := make(map[string]interface{})
	m["id"] = []interface{}{"name", 1}
	vals, err := Eval("-1 * (-(-(10-100)) ^ 2) ^ 3 + 3 * (5 - 17) + 1 + 2", m)
	t.Log("result1 : ", vals, vals.IsString(), err)
	vals, err = Eval("id[1]", m)
	t.Log("result1 : ", vals, vals.IsNumber(), err)
}

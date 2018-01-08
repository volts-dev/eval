package eval

import (
	"testing"
)

func TestMain(t *testing.T) {
	m := make(map[string]interface{})
	m["id"] = []interface{}{"name", 1}
	m["main_object"] = map[string]string{"name": "hzm"}
	vals, err := Eval("main_object and 'name' in main_object and not title and not additional_title", m)
	t.Log("result1 : ", vals, err)
}

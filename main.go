package main

import (
	"fmt"
	"unsafe"
)

type go_iface struct {
	t unsafe.Pointer
	v unsafe.Pointer
}

func a(m any) {
	p := (*go_iface)(unsafe.Pointer(&m))
	to_value := (*int)(p.v)
	*to_value = *to_value + 20
}

type s struct {
	s int
}

type A interface {
	b()
}

func (s s) b() {}

func main() {
	y := &s{10}
	_ = y
	xx := 10
	_ = xx
	zz := []string{"a", "b"}
	_ = zz

	var z2 A = s{}
	_ = z2

	var z3 *A = &z2
	_ = z3

	var m any
	m = z3

	if _, ok := m.(*s); ok {
		p := (*go_iface)(unsafe.Pointer(&m))
		to_value := *(*s)(p.v)
		fmt.Println(to_value.s)
	} else if _, ok := m.(int); ok {
		p := (*go_iface)(unsafe.Pointer(&m))
		to_value := *(*int)(p.v)
		fmt.Println(to_value)
	} else if _, ok := m.([]string); ok {
		p := (*go_iface)(unsafe.Pointer(&m))
		to_value := *(*[]string)(p.v)
		fmt.Println(to_value)
	} else if _, ok := m.(*A); ok {
		p := (*go_iface)(unsafe.Pointer(&m))
		to_value := *(*A)(p.v)
		fmt.Println(to_value)
	}
}

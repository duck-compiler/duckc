package main

import "fmt"

type A struct {
	x int
}

func abc() {
	for _, e := range []string{}{
		e = ""
		_ = e
	}
}

func (self *A) a() {
	self.x = 100
}
func (self A) b(){}

type B struct {
	a **A
}

type C struct {
	c **B
}

type D struct {
	c *C
}

func main() {
	var aa any
	aa = true
	_ = aa

	a := &A{0}
	b := &B{&a}
	x := D{
		&C{
			&b,
		},
	}
	_ = x

	(a).a()

	fmt.Println(a)
}

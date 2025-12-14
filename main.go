package main

import _ "fmt"

type A struct {
	x int
}

type ZZZ struct {
}

func (self ZZZ) ZZFun() {}

type ZZ interface {
	ZZFun()
}

func main() {
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

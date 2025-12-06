package main

import "fmt"

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
	var x any
	x = ZZZ{}
	switch x.(type) {
		case ZZ:
			fmt.Println("is zz")
			break
		case ZZZ:
			fmt.Println("is zz")
			break
		default:
			fmt.Println(1)
	}

	return

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

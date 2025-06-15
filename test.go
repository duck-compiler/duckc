package main

type S struct {}

func x(s S) {}

func main() {
	y := S{ };
	x(y)
}

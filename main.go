package main

import "fmt"
type MyStruct struct {
hallo string
}
type GenericStruct[T any] struct {
hallo T
}
func GenericStruct_map[T any, A any](self *GenericStruct[T], tp T, ap A) A {
return ap
}
func main()  {
var _ MyStruct = MyStruct{hallo: "yoooo"}
var whatever  = GenericStruct[string]{hallo: "yoooo"}
GenericStruct_map[string, string]((&whatever), whatever.hallo, whatever.hallo)
fmt.Println(whatever.hallo)
}

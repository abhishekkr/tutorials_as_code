package main

import (
	"fmt"

	"./somepkg"
)

var (
	num int
	str string
)

func init() {
	num = 10
	str = somepkg.Abc()
}

func Foo() int {
	var (
		n1 int
		n2 int
	)
	return n1 + n2
}

func main() {
	fmt.Println(num, str)
	fmt.Println(Foo())
}

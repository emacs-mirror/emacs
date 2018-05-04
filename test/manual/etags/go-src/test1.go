package main

import "fmt"

type plus interface {
	PrintAdd()
}

type str struct {
	a, b string
}

type intNumber struct {
	a, b int
}

func (s str) PrintAdd() {
	fmt.Println(s.a + s.b)
}

func (n intNumber) PrintAdd() {
	fmt.Println(n.a + n.b)
}

func test(p plus) {
	p.PrintAdd()
}

func main() {
	s := str{a: "Hello,", b: "Emacs!"}
	number := intNumber{a: 1, b: 2}
	test(number)
	test(s)
}

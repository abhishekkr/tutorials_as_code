package main

import (
	"fmt"
	"time"
)

func bitAdd(x, y int) int {
	var carry int
	for y != 0 {
		carry = x & y
		x = x ^ y
		y = carry << 1
	}
	return x
}

func checkBitAdd() {
	bt := time.Now()
	btwo := bitAdd(1, 3)
	bthree := bitAdd(btwo, 5)
	bfour := bitAdd(bthree, 7)
	fmt.Println(btwo, bthree, bfour, time.Since(bt))
}

func justAdd() {
	t := time.Now()
	two := 1 + 3
	three := two + 5
	four := three + 7
	fmt.Println(two, three, four, time.Since(t))
}

func main() {
	justAdd()
	checkBitAdd()
}

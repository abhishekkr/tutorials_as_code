package main

import (
	"fmt"
	"time"
)

func squareByAdd(x int) int {
	result := 0
	y := 1
	for i := 0; i < x; i++ {
		result += y
		y += 2
	}
	return result
}

func checkSquareByAdd() {
	t := time.Now()
	two := squareByAdd(2)
	three := squareByAdd(3)
	four := squareByAdd(4)
	fmt.Println(two, three, four, time.Since(t))
}

func justSquare() {
	t := time.Now()
	two := 2 * 2
	three := 3 * 3
	four := 4 * 4
	fmt.Println(two, three, four, time.Since(t))
}

func main() {
	justSquare()
	checkSquareByAdd()
}

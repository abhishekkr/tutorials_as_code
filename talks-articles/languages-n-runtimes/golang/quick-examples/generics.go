package main

import (
	"fmt"

	"golang.org/x/exp/constraints"
)

type Num interface {
	int | int8 | int16 | int32 | int64 | float32 | float64
}

func Add[K comparable, V int64 | float64](m map[K]V) V {
	var sum V
	for _, val := range m {
		sum += val
	}
	return sum
}

func SumOf[V int64 | float64](m []V) V {
	var sum V
	for _, val := range m {
		sum += val
	}
	return sum
}

func Sum[V Num](m []V) V {
	var sum V
	for _, val := range m {
		sum += val
	}
	return sum
}

func SumSum[V constraints.Ordered](m []V) V {
	var sum V
	for _, val := range m {
		sum += val
	}
	return sum
}

func main() {
	sumOfInt := Add(map[int]int64{0: 10, 1: 20, 2: 30})
	fmt.Println("=>(map)  ", sumOfInt)
	sumOfInt = SumOf([]int64{10, 20, 30})
	fmt.Println("=>(list) ", sumOfInt)
	sumOfInt = Sum([]int64{10, 20, 30})
	fmt.Println("=>(type) ", sumOfInt)
	sumOfInt = SumSum([]int64{10, 20, 30})
	fmt.Println("=>(exp)  ", sumOfInt)
	fmt.Println("done.")
}

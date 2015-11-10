package main

import (
	"fmt"
	"math/rand"
	"reflect"
	"testing/quick"
)

type Point struct{ X, Y int8 }

func main() {
	rnd := rand.New(rand.NewSource(42))
	t := reflect.TypeOf(Point{})
	v, _ := quick.Value(t, rnd)
	fmt.Println("Here's a point:", v)
}

package main

import (
	"fmt"
	"runtime"
	"time"
)

func main() {
	runtime.GOMAXPROCS(10)
	go fmt.Println(1)
	go fmt.Println(11)
	go fmt.Println(111)
	go fmt.Println(1111)
	go fmt.Println(11111)
	go fmt.Println(111111)
	go fmt.Println(1111111)
	runtime.Gosched()

	time.Sleep(time.Second * 2)
}

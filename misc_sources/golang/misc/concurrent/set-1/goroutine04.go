package main

import (
	"fmt"
	"runtime"
	"time"
)

func ten_times(in <-chan int, out chan<- int) {
	for {
		i, ok := <-in
		if !ok {
			break
		}
		out <- 10 * i
	}
}

func main() {
	runtime.GOMAXPROCS(10)
	in := make(chan int, 5)
	out := make(chan int)
	go ten_times(in, out)

	go func() {
		for {
			fmt.Println("~", <-out)
		}
	}()

	in <- 10
	in <- 100
	in <- 1000
	close(in)

	runtime.Gosched()

	time.Sleep(time.Second * 2)
}

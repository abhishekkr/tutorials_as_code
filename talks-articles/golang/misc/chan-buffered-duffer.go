package main

import (
	"fmt"
	"time"
)

func duffer(buffer int) {
	ch := make(chan int, buffer)
	go func() { ch <- 1 }()
	go func() { ch <- 2 }()
	time.Sleep(1 * time.Second)
	fmt.Println("buffer:", buffer, " shows length:", len(ch))
	fmt.Println(<-ch)
	fmt.Println(<-ch)
	time.Sleep(1 * time.Second)
}

func main() {
	duffer(0)
	duffer(1)
	duffer(2)
	duffer(3)
	duffer(4)
}

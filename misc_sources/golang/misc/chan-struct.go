package main

import (
	"fmt"
	"time"
)

type chana struct {
	C1 chan string
	C2 chan string
}

type chanawala struct {
	C chana
}

func c1To2(c *chana) {
	fmt.Println("c1To2")
	c1 := <-c.C1
	c.C2 <- c1
	c1 = <-c.C1
	fmt.Println(c1)
}

func c2To1(c *chana) {
	fmt.Println("c2To1")
	c2 := <-c.C2
	fmt.Println(c2)
	c.C1 <- "new"
}

func main() {
	c := chanawala{}
	c.C.C1 = make(chan string)
	c.C.C2 = make(chan string)
	go c1To2(&(c.C))
	go c2To1(&(c.C))
	c.C.C1 <- "old"
	time.Sleep(time.Second * 2)
}

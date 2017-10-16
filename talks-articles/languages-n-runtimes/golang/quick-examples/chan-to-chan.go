package main

import (
	"fmt"
	"time"
)

func hear(c_hear chan string, c_tell chan string) {
	got := <-c_hear
	fmt.Println("hear:", got)
	c_tell <- got
}
func tell(c_tell chan string, c_retell chan chan string) {
	c_retell <- c_tell
}
func retell(c_hear chan string, c_retell chan chan string) {
	got := <-<-c_retell
	fmt.Println(got)
}

func main() {
	c_hear := make(chan string)
	c_tell := make(chan string)
	c_retell := make(chan chan string)

	fmt.Println("~~~")
	go hear(c_hear, c_tell)
	go tell(c_tell, c_retell)
	go retell(c_hear, c_retell)
	c_hear <- "what"
	time.Sleep(10000)
}

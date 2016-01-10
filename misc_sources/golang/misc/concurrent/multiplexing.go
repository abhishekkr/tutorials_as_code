package main

import (
  "fmt"
  "time"
  "math/rand"
)

/*
Google I/O 2012
Go Concurrency Patterns ~ Rob Pike

"PATTERNS"

Multiplexing : Fan-in function to let anyone ready take the lead
*/

func fanIn(chanl1, chanl2 <- chan string) <-chan string {
  c := make(chan string)
  go func() { for{ c <- <-chanl1 }}()
  go func() { for{ c <- <-chanl2 }}()
  return c
}

func boring(msg string) <-chan string{
  c := make(chan string)
  go func() {
    for i := 0; ; i++ {
      c <- fmt.Sprintf("%s %d", msg, i)
      time.Sleep(time.Duration(rand.Intn(1e3)) * time.Millisecond)
    }
  }()
  return c
}

func main(){
  c := fanIn(boring("Joe"), boring("Ann"))
  for i := 0; i < 5; i++ {
    fmt.Println(<-c)
  }
  fmt.Println("You're both boring; I'm leaving.")
}

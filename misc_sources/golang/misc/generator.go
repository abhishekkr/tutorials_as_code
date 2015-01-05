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

Generator : function that returns a channel
*/

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
  c := boring("boring!")
  for i := 0; i < 5; i++ {
    fmt.Printf("You say %q\n", <-c)
  }
  fmt.Println("You're boring; I'm leaving.")

  joe := boring("Joe")
  ann := boring("Ann")
  for i := 0; i < 5; i++ {
    fmt.Println(<-joe)
    fmt.Println(<-ann)
  }
  fmt.Println("You're both boring; I'm leaving.")
}

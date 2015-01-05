package main

import "fmt"

func main(){
  fmt.Println("check it out")
  defer fmt.Println("first")
  defer fmt.Println("second")
  defer fmt.Println("third")
}

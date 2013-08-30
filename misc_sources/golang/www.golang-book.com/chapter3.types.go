package main

import "fmt"

// to run this $ go run $0

func main(){
  fmt.Println("1 + 1 = ", 1 + 1)
  fmt.Println("1 + 1 = ", 1.0 + 1.0)
  fmt.Println(len("abcde"))
  fmt.Println("abcde"[1:3])
  fmt.Println("ab" + "cde")
  fmt.Println(true && true)
  fmt.Println(true && false)
  fmt.Println(true || true)
  fmt.Println(true || false)
  fmt.Println(! false)
}

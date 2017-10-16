package main

import "fmt"

type funk func(s string)

func PRN(variety string) funk {
    switch variety {
      case "hello":
        return func(some string){ fmt.Println("hello", some) }
      default:
        return func(some string){ fmt.Println(some) }
    }
}

func main(){
  say := PRN("any")
  sayhello := PRN("hello")

  say("oye")
  sayhello("gopher")
}

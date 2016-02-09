package main

import "fmt"

// to run this $ go run $0

func main(){
  fmt.Println(`1
2`)

  i := 1
  for i <= 10 {
    fmt.Print(i)
    i++
  }
  fmt.Println("")

  for i := 1 ; i <= 10 ; i++ {
    fmt.Print(i)
    i++
  }
  fmt.Println("")

  for i := 1 ; i <= 10 ; i++ {
    if i % 5 == 0 {
      fmt.Println(i, " divisible by 5")
    } else if i % 3 == 0 {
      fmt.Println(i, " divisible by 3")
    } else if i % 2 == 0 {
      fmt.Println(i, " divisible by 3")
    } else {
      fmt.Println(i, " divisible by 1")
    }
  }
  fmt.Println("")

  for i := 1 ; i <= 10 ; i++ {
    switch i {
      case 7 : fmt.Println(i, " divisible by 7")
      case 10,5 : fmt.Println(i, " divisible by 5")
      case 9,6,3 : fmt.Println(i, " divisible by 3")
      case 8,4,2 : fmt.Println(i, " divisible by 2")
      default : fmt.Println(i, " divisible by only itself")
    }
  }
  fmt.Println("")
}

package main

import "fmt"

func main(){
  var num int
  fmt.Println("Enter a number:")
  fmt.Scanf("%d", &num)

  switch num {
    case 10:
      fmt.Println("it's 10")
    default:
      fmt.Println("it's not 10")
  }

  switch {
    case num < 10:
      fmt.Println("it's less than 10")
      break
    case num == 10:
      fmt.Println("it's 10")
      break
    case num > 10:
      fmt.Println("it's more than 10")
      fallthrough
    default:
      fmt.Println("it's not 10")
  }
}

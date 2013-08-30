package main

import "fmt"

// to run this $ go run $0

func abc (xx *int){
  *xx += 1
}

func main(){
  x := 1
  abc(&x)
  fmt.Println(x)

  y := new(int)
  abc(y)
  fmt.Println(*y)
}

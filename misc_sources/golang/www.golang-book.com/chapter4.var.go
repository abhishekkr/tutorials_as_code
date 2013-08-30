package main

import "fmt"

// to run this $ go run $0

var (
  abc = "ABC"
  def = "DEF"
)

func askAndDouble(){
  fmt.Print("Enter a number: ")
  var in float64
  fmt.Scanf("%f", &in)
  out := in * 2
  fmt.Println("Double of", in, "is", out)
  in *= 2
  fmt.Println(in == out)
}

func main(){
  var x string = "Raguel"
  fmt.Println(x)

  var y string
  y = "Rag"
  fmt.Println(y)
  y = y + "uel"
  fmt.Println(y)

  fmt.Println(x == y)

  z := "Robin"
  fmt.Println(z)

  a := "Hood"
  fmt.Println(a)

  fmt.Println(abc)

  {
    xyz := "ABCDE"
    fmt.Println(xyz)
  }
  /* fmt.Println(xyz) // can't use here */

  const uvw = "UVW"
  fmt.Println(uvw)

  askAndDouble()
}

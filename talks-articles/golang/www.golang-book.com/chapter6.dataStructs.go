package main

import "fmt"

// to run this $ go run $0

func main(){
  //ARRAYS
  fmt.Println("ARRAYS")
  var x [5]int
  x[4] = 100
  fmt.Println(x)

  for i := 0; i < len(x); i++ {
    x[i] = x[4] + i
  }
  fmt.Println(x)

  var total float64 = 0
  for _, value := range x {
    total += float64(value)
  }
  fmt.Println(total / float64(len(x)))

  y := [5]float64{ 1, 2, 3, 4, 5, }
  for i, value := range y {
    fmt.Println("at", i, "value is", value)
  }

  //SLICES
  fmt.Println("SLICES")
  z := []float64{1, 2, 3, 4, 5, 6, 7}
  a := make([]float64, 2)
  b := make([]float64, 3, 5)
  copy(a, z)
  copy(b, z)
  fmt.Println(a, b)
  c := append(a, 10, 11)
  fmt.Println(c)

  //MAPS
  fmt.Println("MAPS")
  var d = make(map[string]string)
  d["a"] = "A"
  d["b"] = "B"
  fmt.Println(d)

  var e = make(map[int]int)
  e[5] = 10
  e[7] = 70
  fmt.Println(e)
  delete(e, 7)
  fmt.Println(e)
  count, ok := e[5]
  fmt.Println(count, ok)
  count2, ok2 := e[7]
  fmt.Println(count2, ok2)
  if count, ok := e[5]; ok {
    fmt.Println(count)
  }

  elems := map[string]string{
    "aa": "AA",
    "bb": "BB",
  }
  fmt.Println(elems)

  elems2 := map[string]map[string]string{
    "a": {
          "aa": "AA",
        },
    "b": {
          "bb": "BB",
        },
  }
  fmt.Println(elems2)
}

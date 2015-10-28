package main

import (
  "fmt"
)


func gimmeAnythingToPrint(xyz interface{}){
  fmt.Println(xyz)
}


func whoAmI(xyz interface{}){
  switch v := xyz.(type) {
    case string:
      fmt.Println(v)
    case int, int32, int64:
      fmt.Println(">", v)
    default:
      fmt.Println("dunno")
  }
}


func interface01(){
  var anyvar01 interface{} = "string"
  var anyvar02 interface{} = 100
  gimmeAnythingToPrint(anyvar01)
  gimmeAnythingToPrint(anyvar02)
  whoAmI(anyvar01)
  whoAmI(anyvar02)
}


func main(){
  interface01()
}

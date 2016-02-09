package main

import "fmt"

// to run this $ go run $0

type Hashmap map[string]string

type Box struct { //structs def
  len int32
  wid int32
}

type Figure struct{
  sides int32
  measures map[int32]int32
}

func notChange (b Box) {
  b.len += 1
}

func doChange (b *Box) {
  b.len += 1
}

/* methods
func (RECEIVER RECEIVER_TYPE) funcName....
*/
func (b *Box) area() int32 {
  return b.len * b.wid
}

/* Embedded Type
Type A is Type B rather than Type A has Type B
*/
type rectangle struct {
  Box
  r_area int32
}

type UUID int32

//main
func main(){
  hm := make(Hashmap)
  hm["a"] = "A"
  fmt.Println(hm)

  b1 := Box{len: 10, wid: 15} //structs use
  fmt.Println(b1)

  b2 := Box{1, 5}
  fmt.Println(b2.len, b2.wid)

  notChange(b2)
  fmt.Println(b2.len, b2.wid)

  doChange(&b2)
  fmt.Println(b2.len, b2.wid)

  fmt.Println(b2.area()) // go automatically pass a pointer at Methods

  r := new(rectangle) // struct use
  r.len = 10
  r.wid = 100
  r.r_area = r.area()
  fmt.Println(r)

  var f Figure // structs use
  fmt.Println(f)

  var my_uuid UUID
  my_uuid = 12345
  fmt.Println(my_uuid)

  fmt.Println("")
}

package main

import "fmt"

// to run this $ go run $0

type Num2 struct {
  n1, n2 int32
}
func (n *Num2) sum() int32 {
  return n.n1 + n.n2
}

type Num3 struct {
  n1, n2, n3 int32
}
func (n *Num3) sum() int32 {
  return n.n1 + n.n2 + n.n3
}

type Num interface{   //interface for all numbers
  sum() int32
}
func totalSum(nums ...Num) int32 {
  var sum int32
  for _, s := range nums {
    sum += s.sum()
  }
  return sum
}

type MultiNum struct { // it can contain Num2, Num3 and MultiNum
  nums []Num
}
func (multiNum *MultiNum) sum() int32{
  var sum int32
  for _, n := range multiNum.nums {
    sum += n.sum()
  }
  return sum
}

func main(){
  num2 := Num2{1, 2}
  fmt.Println(num2.sum())
  fmt.Println("")

  num3 := Num3{1, 2, 3}
  fmt.Println(num3.sum())
  fmt.Println("")

  fmt.Println(totalSum(&num2, &num3))
  fmt.Println("")

  m1 := MultiNum{[]Num{&num2, &num3, }}
  fmt.Println(m1.sum())
  fmt.Println("")
}

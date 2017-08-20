package main

import "fmt"

// to run this $ go run $0

func average (xs []int) int {
  total := 0
  for _, val := range xs {
    total += val
  }
  return total / len(xs)
}

func average2 (xs []int) (avg int) { //named return type
  total := 0
  for _, val := range xs {
    total += val
  }
  avg = total / len(xs)
  return
}

func average3 (xs []int) (int, int) { //multiple return
  total := 0
  for _, val := range xs {
    total += val
  }
  avg := total / len(xs)
  return total, avg
}

func average4 (xs []int) (avg int, total int) { //multiple named return
  for _, val := range xs {
    total += val
  }
  avg = total / len(xs)
  return
}

func add(msg string, args ...int) int { //varadic func, last param
  total := 0
  for _, val := range args {
    total += val
  }
  return total
}

func dbl() func() uint{
  cnt := uint(0)
  return func() (ret uint) {
    ret = cnt
    cnt += 2
    return
  }
}

func retBefore1 (i int) {
  fmt.Println(i)
  if i <= 1 {
    return
  }
  i -= 1
  retBefore1(i)
}

func main(){
  xs := []int{1, 2, 3, 4, 5}
  fmt.Println(average(xs))
  fmt.Println(average2(xs))

  fmt.Println(average3(xs))
  fmt.Println(average4(xs))

  fmt.Println(add("sum", 1, 2, 3, 4, 5, 6))
  fmt.Println(add("sum", xs...)) //slice here as well

  sub := func(aa, bb int) int { //closure
    return aa - bb
  }
  fmt.Println(sub(10, 5))
  aaa := 0
  inc := func() int { //closure
    aaa++
    return aaa
  }
  fmt.Println(inc())
  fmt.Println(inc())

  retBefore1(5)

  fmt.Println(dbl()())
  fmt.Println(dbl()())
  dbl2 := dbl()
  fmt.Println(dbl2())
  fmt.Println(dbl2())
  defer fmt.Println(dbl2()) //deferred to end
  defer fmt.Println(dbl2()) //deferred to end

  fmt.Println(dbl2())

  defer func(){
    str := recover()
    fmt.Println(str)
  }()
  panic("PANIC")
}

package main

import (
  "fmt"
)

func main(){
  s1 := []int{1, 2, 3, 4, 5, 6, 7}
  fmt.Println("s1", s1)

  var s2 []int
  s2 = make([]int, 5) // of length 5
  s2 = s1[2:5]
  fmt.Println("s2:no-limit", s2)
  s2 = s1[2:5:7]
  s2 = append(s2, 8)
  s2 = append(s2, 9)
  fmt.Println("s2", s2)

  var s3 []int
  s3 = make([]int, 5, 10) // of length 5, max limit 10, extensible
  fmt.Println("s3:default", s3)
  s3 = append(s3, 10)
  s3 = append(s3, s2...)
  fmt.Println("s3", s3)

  s4 := []int{11, 12, 13, 14}
  fmt.Printf("Before 'copy(s1, s4[1:3])': s1:(%v) and s4:(%v)\n", s1, s4)
  copy(s1, s4[1:3])
  fmt.Printf("s1:(%v) and s4:(%v)\n", s1, s4)

  fmt.Printf("Before 'copy(s4, s1)': s1:(%v) and s4:(%v)\n", s1, s4)
  copy(s4, s1)
  fmt.Printf("s1:(%v) and s4:(%v)\n", s1, s4)

  fmt.Printf("Before 'copy(s1, s2[1:3])': s1:(%v) and s2:(%v)\n", s1, s2)
  copy(s1, s2[1:3])
  fmt.Printf("s1:(%v) and s2:(%v)\n", s1, s2)

  fmt.Printf("Before 'copy(s2, s1)': s1:(%v) and s2:(%v)\n", s1, s2)
  copy(s2, s1)
  fmt.Printf("s1:(%v) and s2:(%v)\n", s1, s2)
  fmt.Println("s1 again changed 'cuz 'copy can handle source and destination slices that share the same underlying array, handling overlapping slices correctly'")
}

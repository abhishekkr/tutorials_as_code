package main

import "fmt"

type Person interface {
  MyName()
  TheirName()
}
type FirstPerson interface {
  MyName()
}
type ThirdPerson interface {
  TheirName()
}

type Man struct {
  name string
}
func (man Man) MyName() {
  fmt.Println("I'm a man and my name is", man.Name)
}
func (man Man) TheirName() {
  fmt.Println("His name is", man.Name)
}

type Woman struct {
  name string
}
func (woman Woman) MyName() {
  fmt.Println("I'm a woman and my name is", woman.Name)
}
func (woman Woman) TheirName() {
  fmt.Println("Her name is", woman.Name)
}

func main(){
  var fp FirstPerson
  fp = new(Man)
  fp.MyName()
  fp.(ThirdPerson).TheirName()
}

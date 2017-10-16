package main

import "fmt"

type Player interface {
  MyName()
  TheirName()
}
type FirstPerson interface {
  MyName()
}
type ThirdPerson interface {
  TheirName()
}

type Man struct {}
func (man Man) MyName() {
  fmt.Println("What's your name dude?")
}
func (man Man) TheirName() {
  fmt.Println("What's his name?")
}

type Woman struct {}
func (woman Woman) MyName() {
  fmt.Println("What's your name Lady?")
}
func (woman Woman) TheirName() {
  fmt.Println("What's her name?")
}

type Human struct {
  HumanPlayer Player
  Name string
}
func (human *Human) ConfigPlayer(mode Player) {
  human.HumanPlayer = mode
}

func main(){
  player00 := Human{HumanPlayer: new(Man)}
  player01 := Human{HumanPlayer: new(Woman)}

  player00.HumanPlayer.MyName()
  fmt.Scanf("%s", &player00.Name)

  player01.HumanPlayer.TheirName()
  fmt.Scanf("%s", &player01.Name)

  fmt.Printf("MortalKombat: %s vs. %s\n", player00.Name, player01.Name)
}

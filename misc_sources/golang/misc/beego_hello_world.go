package main

import (
  "github.com/astaxie/beego"
)

type MainController struct {
  beego.Controller
}

func (this *MainController) Get() {
  this.Ctx.WriteString("hello world")
}

func main() {
  println("By Default this Beego example would run at 127.0.0.1:8080")
  beego.Router("/", &MainController{})
  beego.Run()
}

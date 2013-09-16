package main

import (
  "fmt"
)
import (
  "./client"
  "./server"
)

func main(){
  fmt.Println("starting Echo server...")
  go server.Echo()
  fmt.Println("starting Echo client...")
  client.Echo()
}

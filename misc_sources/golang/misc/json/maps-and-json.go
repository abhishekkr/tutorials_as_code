package main

import (
  "fmt"
  "encoding/json"
)

func main(){
  rhyme := map[string]string{"to": "do", "fun": "pun"}
  fmt.Println(rhyme)

  rhyme["jive"] = "five"
  rhyme["kill"] = "bill"
  fmt.Println(rhyme)

  delete(rhyme, "to")
  fmt.Println(rhyme)

  var chime map[string]string
  chime = make(map[string]string, 10)
  chime = rhyme
  fmt.Println(chime)

  eg_json, _ := json.Marshal(chime)
  fmt.Println(eg_json)

  var prime map[string]string
  prime = make(map[string]string)
  status := json.Unmarshal(eg_json, &prime)
  fmt.Println(status, prime)
}

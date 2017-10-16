package main

import (
	"encoding/json"
	"fmt"
)

type Company struct {
	Name    string `json:"person_name"`
	Address `json:"person_address_obj"`
}

type Address struct {
	City  string `json:"person_city"`
	State string `json:"person_state"`
}

func main() {
	p := Company{
		Name: "Anon ymous",
		Address: Address{
			City:  "Craft",
			State: "Spot",
		},
	}
	b, _ := json.Marshal(p)
	fmt.Println(string(b))
}

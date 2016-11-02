package main

import "fmt"

var (
	person struct {
		name struct {
			firstname  string
			middlename string
			lastname   string
		}
	}
)

type empty struct{}
type node struct {
	edges        []string
	uuid, weight int
}
type location struct {
	street      string
	city, state string
	postal      string
}
type company struct {
	name    string
	address location
}
type shop struct {
	name string
	location
}

func updateCmpny(c company, name string) {
	c.name = name
}
func updateCmpnyReal(c *company, name string) {
	c.name = name
}

func main() {
	what := struct {
		ever string
	}{
		"hey hey my my",
	}
	fmt.Println(what.ever)

	person.name.firstname = "anonymous"
	fmt.Println(person.name.firstname)
	fmt.Println(person.name.lastname)

	somecmpny := company{
		"some place",
		location{"1st Random", "Craft", "Spot", "10"},
	}
	fmt.Println(somecmpny.address.city)

	someshop := shop{
		name:     "some place",
		location: location{"1st Random", "Craft", "Spot", "10"},
	}
	fmt.Println(someshop.location.city)
	fmt.Println(someshop.city)

	updateCmpny(somecmpny, "some cmpny")
	fmt.Println(somecmpny.name)

	updateCmpnyReal(&somecmpny, "some cmpny")
	fmt.Println(somecmpny.name)

	cmpny := new(company)
	updateCmpnyReal(cmpny, "some cmpny")
	fmt.Println(cmpny.name)
}

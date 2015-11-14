package main

import "fmt"

func abc(a string, b bool) bool {
	fmt.Println(a)
	return b
}

func main() {
	fmt.Println("showing the evaluation of expression in a condition, the direction and flow-break")

	fmt.Println("true and true")
	if abc("1", true) && abc("2", true) {
		fmt.Println("yay")
	}

	fmt.Println("true and false")
	if abc("1", true) && abc("2", false) {
		fmt.Println("yay")
	}

	fmt.Println("false and true")
	if abc("1", false) && abc("2", true) {
		fmt.Println("yay")
	}

	fmt.Println("false and false")
	if abc("1", false) && abc("2", false) {
		fmt.Println("yay")
	}

	fmt.Println("true or true")
	if abc("1", true) || abc("2", true) {
		fmt.Println("yay")
	}

	fmt.Println("true or false")
	if abc("1", true) || abc("2", false) {
		fmt.Println("yay")
	}

	fmt.Println("false or true")
	if abc("1", false) || abc("2", true) {
		fmt.Println("yay")
	}

	fmt.Println("false or false")
	if abc("1", false) || abc("2", false) {
		fmt.Println("yay")
	}

	fmt.Println("flow is Left-to-Right and it breaks at the first expression that can be decisive for condition")
}

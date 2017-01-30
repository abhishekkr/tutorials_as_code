package main

import "fmt"

func loopr() {
	var oye string
loop:
	for {
		fmt.Scanf("%s", &oye)
		switch oye {
		case "a":
			fmt.Println("+++", oye)
			break

		case "b":
			fmt.Println("***", oye)
			break loop

		default:
			fmt.Println(">>>>>", oye)
		}
	}
}

func main() {
	loopr()
}

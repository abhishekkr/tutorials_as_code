package main

/*
Let's write a fun little program to print Xmas Tree on Console.
*/

import "fmt"

var (
	greenTxtTemplate = "\x1b[32m%s\x1b[0m"
	redTxtTemplate   = "\x1b[31m%s\x1b[0m"
	leafItem         = "x"
	trunkItem        = "t"
	maxWidth         = 50
)

func printFestive(item string, tmpl string, thickness int, width int) {
	whitespace := width - thickness
	sidespace := int(whitespace / 2)
	for idx := 0; idx < sidespace; idx++ {
		fmt.Print(" ")
	}
	txt := ""
	for idx := 0; idx < thickness; idx++ {
		txt += item
	}
	toPrint := fmt.Sprintf(tmpl, txt)
	fmt.Println(toPrint)
}

func main() {
	for idx := 0; idx < 3; idx++ {
		baseCount := (idx * 10) + 1
		maxCount := ((idx + 1) * 10) + 6
		for leafCount := baseCount; leafCount < maxCount; leafCount += 2 {
			printFestive(leafItem, greenTxtTemplate, leafCount, maxWidth)
		}
	}
	for count := 0; count < 10; count++ {
		printFestive(trunkItem, redTxtTemplate, 3, maxWidth)
	}
}

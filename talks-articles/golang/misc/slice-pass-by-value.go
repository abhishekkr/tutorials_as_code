package main

import "fmt"

func reverse01(s []int) {
	for i, j := 0, len(s)-1; i < j; i++ {
		j = len(s) - (i + 1)
		s[i], s[j] = s[j], s[i]
	}
}

func reverse02(s []int) {
	s = append(s, 999)
	for i, j := 0, len(s)-1; i < j; i++ {
		j = len(s) - (i + 1)
		s[i], s[j] = s[j], s[i]
	}
}

func reverse03(s []int) {
	s = append(s, 997)
	s = append(s, 998)
	s = append(s, 999)
	for i, j := 0, len(s)-1; i < j; i++ {
		j = len(s) - (i + 1)
		s[i], s[j] = s[j], s[i]
	}
}

func reverse04(s []int) {
	newElem := 999
	for len(s) < cap(s) {
		fmt.Println("Adding an element:", newElem, "cap:", cap(s), "len:", len(s))
		s = append(s, newElem)
		newElem++
	}
	for i, j := 0, len(s)-1; i < j; i++ {
		j = len(s) - (i + 1)
		s[i], s[j] = s[j], s[i]
	}
}

func main() {
	var s = []int{1, 2, 3, 4, 5, 6, 7, 8, 9}
	reverse01(s)
	fmt.Println(s)
	reverse02(s)
	fmt.Println(s)
	reverse03(s)
	fmt.Println(s)
	reverse04(s)
	fmt.Println(s)
}

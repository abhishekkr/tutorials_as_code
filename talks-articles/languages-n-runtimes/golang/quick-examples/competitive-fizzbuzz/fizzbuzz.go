package main

import "strconv"

func FizzBuzzModuloA(upto int) []string {
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		num := idx + 1
		if num%15 == 0 {
			fizzbuzzList[idx] = "fizzbuzz"
		} else if num%5 == 0 {
			fizzbuzzList[idx] = "buzz"
		} else if num%3 == 0 {
			fizzbuzzList[idx] = "fizz"
		} else {
			fizzbuzzList[idx] = strconv.Itoa(num)
		}
	}
	return fizzbuzzList
}

func FizzBuzzModuloB(upto int) []string {
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		num := idx + 1
		if num%3 == 0 {
			fizzbuzzList[idx] += "fizz"
		}
		if num%5 == 0 {
			fizzbuzzList[idx] += "buzz"
		}

		if fizzbuzzList[idx] == "" {
			fizzbuzzList[idx] = strconv.Itoa(num)
		}
	}
	return fizzbuzzList
}

func FizzBuzzModuloC(upto int) []string {
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		num := idx + 1
		vanilla := true
		if num%3 == 0 {
			fizzbuzzList[idx] += "fizz"
			vanilla = false
		}
		if num%5 == 0 {
			fizzbuzzList[idx] += "buzz"
			vanilla = false
		}

		if vanilla {
			fizzbuzzList[idx] = strconv.Itoa(num)
		}
	}
	return fizzbuzzList
}

func FizzBuzzModuloD(upto int) []string {
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		num := idx + 1
		byThree := num%3 == 0
		byFive := num%5 == 0
		if byThree && byFive {
			fizzbuzzList[idx] = "fizzbuzz"
		} else if byFive {
			fizzbuzzList[idx] = "buzz"
		} else if byThree {
			fizzbuzzList[idx] = "fizz"
		} else {
			fizzbuzzList[idx] = strconv.Itoa(num)
		}
	}
	return fizzbuzzList
}

func FizzBuzzModuloE(upto int) []string {
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		num := idx + 1
		fizzbuzzList[idx] = strconv.Itoa(num)
		if num%3 == 0 {
			fizzbuzzList[idx] = "fizz"
			if num%5 == 0 {
				fizzbuzzList[idx] += "buzz"
			}
		} else if num%5 == 0 {
			fizzbuzzList[idx] = "buzz"
		}
	}
	return fizzbuzzList
}

func FizzBuzzIncAndMultiply(upto int) []string {
	forThree := 1
	forFive := 1
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		num := idx + 1
		if (num/(3*forThree) == 1) && (num/(5*forFive) == 1) {
			fizzbuzzList[idx] = "fizzbuzz"
			forThree++
			forFive++
		} else if num/(5*forFive) == 1 {
			fizzbuzzList[idx] = "buzz"
			forFive++
		} else if num/(3*forThree) == 1 {
			fizzbuzzList[idx] = "fizz"
			forThree++
		} else {
			fizzbuzzList[idx] = strconv.Itoa(idx + 1)
		}
	}
	return fizzbuzzList
}

func FizzBuzzIncremental(upto int) []string {
	byThree := 0
	byFive := 0
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		byThree++
		byFive++
		if byThree == 3 && byFive == 5 {
			fizzbuzzList[idx] = "fizzbuzz"
			byFive = 0
			byThree = 0
		} else if byFive == 5 {
			fizzbuzzList[idx] = "buzz"
			byFive = 0
		} else if byThree == 3 {
			fizzbuzzList[idx] = "fizz"
			byThree = 0
		} else {
			fizzbuzzList[idx] = strconv.Itoa(idx + 1)
		}
	}
	return fizzbuzzList
}

func FizzBuzzModuloDGo(upto int) []string {
	fn := func(num int) string {
		byThree := num%3 == 0
		byFive := num%5 == 0
		if byThree && byFive {
			return "fizzbuzz"
		} else if byFive {
			return "buzz"
		} else if byThree {
			return "fizz"
		} else {
			return strconv.Itoa(num)
		}
	}
	fizzbuzzList := make([]string, upto)
	for idx := 0; idx < upto; idx++ {
		fizzbuzzList[idx] = go fn(idx + 1)
	}
	return fizzbuzzList
}

func main() {
	FizzBuzzModuloA(30)
	FizzBuzzModuloB(30)
	FizzBuzzModuloC(30)
	FizzBuzzModuloD(30)
	FizzBuzzModuloE(30)
	FizzBuzzIncAndMultiply(30)
	FizzBuzzIncremental(30)
	FizzBuzzModuloDGo(30)
}

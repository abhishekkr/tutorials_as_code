package main

import (
	"fmt"
	"time"

	"runtime"
	"sync"
)

var (
	money int
	vault sync.Mutex
)

func payHimAlready(pay int, c chan int) {
	fmt.Println("deposit money", pay)
	c <- +pay
}

func payBillsNow(bill int, c chan int) {
	fmt.Println("deduct money", bill)
	c <- -bill
}

func main() {
	runtime.GOMAXPROCS(10)

	amounts := []int{100, 10, 55, 5, 100}

	c := make(chan int, len(amounts))

	for idx, amount := range amounts {
		switch idx {
		case 0, 4:
			go payHimAlready(amount, c)
		default:
			go payBillsNow(amount, c)
		}
		go func() { money = money + <-c }()
		fmt.Println("$$", money)
	}
	runtime.Gosched()

	time.Sleep(time.Second * 2)
	fmt.Println(money)
}

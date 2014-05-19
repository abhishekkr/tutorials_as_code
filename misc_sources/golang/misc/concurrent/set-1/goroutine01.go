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

func payHimAlready(pay int) {
	vault.Lock()
	money += pay
	vault.Unlock()
}

func payBillsNow(bill int) {
	vault.Lock()
	money -= bill
	vault.Unlock()
}

func main() {
	runtime.GOMAXPROCS(10)
	go payHimAlready(100)
	go payBillsNow(10)
	go payBillsNow(55)
	go payBillsNow(5)
	go payHimAlready(100)
	time.Sleep(time.Second * 2)
	fmt.Println(money)
}

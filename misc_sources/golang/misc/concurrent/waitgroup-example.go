package main

/*
source: http://mindfsck.net/example-golang-makes-concurrent-programming-easy-awesome/
*/

import (
	"fmt"
	"sync"
	"time"
)

func f(wg *sync.WaitGroup, val string) {
	time.Sleep(3 * time.Second) //Sleep a few secs
	fmt.Printf("Finished: %v - %v\n", val, time.Now())
	wg.Done()
}

func main() {

	var wg sync.WaitGroup

	wg.Add(3) //We need to wait for 3 calls to 'done' on this wait group

	go f(&wg, "goroutine A") //Call function f concurrently

	//Anonymous func / closure
	go func(wg *sync.WaitGroup, val string) {
		time.Sleep(3 * time.Second) //Sleep a few secs
		fmt.Printf("Finished: %v - %v\n", val, time.Now())
		wg.Done()
	}(&wg, "goroutine B") //Call Anonymous function concurrently

	go f(&wg, "goroutine C") //Call function f concurrently

	wg.Wait() //Wait for the concurrent routines to call 'done'

	fmt.Printf("Finished all goroutines: %v\n", time.Now())
}

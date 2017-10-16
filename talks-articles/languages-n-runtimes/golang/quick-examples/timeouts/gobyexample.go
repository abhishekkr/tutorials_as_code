/*
it is modified
original at source: https://gobyexample.com/timeouts
*/

package main

import "time"
import "fmt"

func delayPingChannel(x chan string, ping string, seconds int) {
	time.Sleep(time.Second * time.Duration(seconds))
	x <- ping
}

func channelActionWithTimeout(x chan string, timeout int, timeoutMsg string) {
	select {
	case res := <-x:
		fmt.Println(res)
	case <-time.After(time.Second * time.Duration(timeout)):
		fmt.Println(timeoutMsg)
	}
}

func main() {

	c1 := make(chan string, 1)
	go delayPingChannel(c1, "result-1", 2)
	channelActionWithTimeout(c1, 1, "timeout-1")

	c2 := make(chan string, 1)
	go delayPingChannel(c2, "result-2", 2)
	channelActionWithTimeout(c2, 3, "timeout-2")
}

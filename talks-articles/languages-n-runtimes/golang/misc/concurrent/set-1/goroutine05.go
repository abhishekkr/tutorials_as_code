package main

import (
	"fmt"
	"runtime"
	"strings"
	"time"
)

func ChurnTwitter(dat string) string {
	return strings.Fields(dat)[0]
}

func ChurnFacebook(dat string) string {
	return strings.Fields(dat)[1]
}

func FanInDataStreams(twitter, facebook <-chan string) chan string {
	out := make(chan string)
	go func() {
		for {
			select {
			case dat := <-twitter:
				out <- ChurnTwitter(dat)
			case dat := <-facebook:
				out <- ChurnFacebook(dat)
			}
		}
	}()
	return out
}

func main() {
	runtime.GOMAXPROCS(10)
	twitter := make(chan string, 5)
	facebook := make(chan string, 5)

	dataminer := FanInDataStreams(twitter, facebook)

	twitter <- "chirp anythin"
	facebook <- "anythin like"
	twitter <- "DM whatever"
	twitter <- "follow alike"
	facebook <- "try share"

	for {
		select {
		case nfo := <-dataminer:
			fmt.Println("~~", nfo)
		case <-time.After(time.Second * 2):
			fmt.Println("sayonara")
			return
		}
	}

	runtime.Gosched()
}

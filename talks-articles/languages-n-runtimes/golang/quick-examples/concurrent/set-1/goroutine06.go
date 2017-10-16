package main

import (
	"fmt"
	"runtime"
)

type PushToFunk func(dat string) string

func PushToTwitter(dat string) string {
	return "Tweeted: " + dat
}

func PushToFacebook(dat string) string {
	return "FBPosted: " + dat
}

func FanOutPosts(funk PushToFunk, in <-chan string, out chan<- string) {
	go func() {
		for {
			dat, ok := <-in
			if !ok {
				out <- "No more posting now!" + dat
				return
			}
			out <- funk(dat)
		}
	}()
}

func main() {
	runtime.GOMAXPROCS(10)
	posts := make(chan string, 15)
	result := make(chan string, 10)

	destinations := []PushToFunk{PushToTwitter, PushToFacebook}

	for _, dests := range destinations {
		go FanOutPosts(dests, posts, result)
	}

	go func() {
		for {
			nfo, ok := <-result
			if !ok {
				return
			}
			fmt.Println(nfo)
		}
	}()

	posts <- "chirp anythin"
	posts <- "anythin like"
	posts <- "DM whatever"
	posts <- "follow alike"
	posts <- "try share"
	close(posts)

	runtime.Gosched()
}

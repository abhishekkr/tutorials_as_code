package faker

import (
	"fmt"
	"math/rand"
	"time"
)

var (
	Web   = FakeSearch("web")
	Image = FakeSearch("image")
	Video = FakeSearch("video")
)

type Result string
type Search func(query string) Result

func FakeSearch(kind string) Search {
	return func(query string) Result {
		sleep_for := rand.Intn(100)
		time.Sleep(time.Duration(sleep_for) * time.Millisecond)
		return Result(fmt.Sprintf("%s result for %q (sleepy: %d)\n", kind, query, sleep_for))
	}
}

// v1
func GoogleSequential(query string) (results []Result) {
	results = append(results, Web(query))
	results = append(results, Image(query))
	results = append(results, Video(query))
	return
}

// v2
func GoogleMinimalTime(query string) (results []Result) {
	c := make(chan Result)
	go func() { c <- Web(query) }()
	go func() { c <- Image(query) }()
	go func() { c <- Video(query) }()

	for i := 0; i < 3; i++ {
		result := <-c
		results = append(results, result)
	}
	return
}

// v2.1
func GoogleAvoidSlow(query string) (results []Result) {
	c := make(chan Result)
	go func() { c <- Web(query) }()
	go func() { c <- Image(query) }()
	go func() { c <- Video(query) }()

	timeout := time.After(80 * time.Millisecond)
	for i := 0; i < 3; i++ {
		select {
		case result := <-c:
			results = append(results, result)
		case <-timeout:
			fmt.Println("timed out")
			return
		}
	}
	return
}

// fillers: replicate servers, send request to multiple replicas and use first response
func GoogleFirst(query string, replicas ...Search) Result {
	c := make(chan Result)
	searchReplica := func(i int) { c <- replicas[i](query) }

	for i := range replicas {
		go searchReplica(i)
	}
	return <-c
}

// v3
func GoogleReplicaAndLatency(query string) (results []Result) {
	Web1 := FakeSearch("web1")
	Web2 := FakeSearch("web2")
	Image1 := FakeSearch("image1")
	Image2 := FakeSearch("image2")
	Video1 := FakeSearch("video1")
	Video2 := FakeSearch("video2")

	c := make(chan Result)
	go func() { c <- GoogleFirst(query, Web1, Web2) }()
	go func() { c <- GoogleFirst(query, Image1, Image2) }()
	go func() { c <- GoogleFirst(query, Video1, Video2) }()

	timeout := time.After(80 * time.Millisecond)
	for i := 0; i < 3; i++ {
		select {
		case result := <-c:
			results = append(results, result)
		case <-timeout:
			fmt.Println("timed out")
			return
		}
	}
	return
}

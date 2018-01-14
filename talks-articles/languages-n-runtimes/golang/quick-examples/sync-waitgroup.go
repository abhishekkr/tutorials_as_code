package main

import (
	"fmt"
	"log"
	"net/http"
	"sync"
	"time"
)

func sleepFun(sec time.Duration, wg *sync.WaitGroup) {
	defer wg.Done()
	time.Sleep(sec * time.Second)
	fmt.Println("goroutine exit")
}

func sleeper() {
	var wg sync.WaitGroup

	wg.Add(2)
	go sleepFun(1, &wg)
	go sleepFun(3, &wg)
	wg.Wait()
	fmt.Println("Main goroutine exit")
}

type Foo struct{}

func (f *Foo) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	w.Write([]byte("Foo"))
}

type Bar struct{}

func (f *Bar) ServeHTTP(w http.ResponseWriter, req *http.Request) {
	w.Write([]byte("Bar"))
}

func server() {
	wg := &sync.WaitGroup{}
	wg.Add(1)
	go func() {
		log.Fatal(http.ListenAndServe(":8081", &Bar{}))
		wg.Done()
	}()
	wg.Add(1)
	go func() {
		log.Fatal(http.ListenAndServe(":8080", &Foo{}))
		wg.Done()
	}()
	wg.Wait()
	fmt.Println("Main goroutine exit")
}

func main() {
	sleeper()
	server()
}

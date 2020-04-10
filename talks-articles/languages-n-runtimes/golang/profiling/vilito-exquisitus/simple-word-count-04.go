package main

import (
	"bufio"
	"flag"
	"fmt"
	"log"
	"os"
	"runtime"
	"runtime/pprof"
	"strings"
	"sync"
	"time"
)

// implementation: concurrent with a work queue producing partial results, reducing final outputs to one

type countMap map[string]int

var (
	cpuprofile   = flag.String("cpuprofile", "", "write cpu profile to `file`")
	memprofile   = flag.String("memprofile", "", "write mem profile to `file`")
	workersCount = flag.Int("workers", 4, "workers to queue for processing")
)

func main() {
	if *cpuprofile != "" {
		f, err := os.Create(*cpuprofile)
		if err != nil {
			log.Fatal("could not create CPU profile: ", err)
		}
		if err := pprof.StartCPUProfile(f); err != nil {
			log.Fatal("could not start CPU profile: ", err)
		}
		defer pprof.StopCPUProfile()
	}

	wordCount()

	if *memprofile != "" {
		f, err := os.Create(*memprofile)
		if err != nil {
			log.Fatal("could not create memory profile: ", err)
		}
		runtime.GC()
		if err := pprof.WriteHeapProfile(f); err != nil {
			log.Fatal("could not write memory profile: ", err)
		}
		f.Close()
	}
}

func wordCount() {
	flag.Parse()

	if len(flag.Args()) == 0 {
		fmt.Println("No files to process")
		return
	}

	result := make(countMap)
	workerWG := new(sync.WaitGroup)
	workerResult := make(chan countMap, *workersCount)
	workerQueue := make(chan string, *workersCount)
	reducerWG := new(sync.WaitGroup)

	start := time.Now()
	reducer(reducerWG, result, workerResult)
	for idx := 0; idx < *workersCount; idx++ {
		processFile(workerWG, workerResult, workerQueue)
	}
	for _, filename := range flag.Args() {
		workerQueue <- filename
	}
	close(workerQueue)
	workerWG.Wait()
	close(workerResult)
	reducerWG.Wait()

	//printResult(result)
	fmt.Printf("Processing took: %v\n", time.Since(start))
}

func processFile(workerWG *sync.WaitGroup, result chan<- countMap, workerQueue <-chan string) {
	workerWG.Add(1)
	go func() {
		var word string
		for filename := range workerQueue {
			reader, err := os.Open(filename)
			if nil != err {
				fmt.Println(err)
				return
			}
			defer reader.Close()

			scanner := bufio.NewScanner(reader)
			scanner.Split(bufio.ScanWords)

			partialResult := make(countMap)
			for scanner.Scan() {
				word = strings.ToLower(scanner.Text())
				partialResult[word] = partialResult[word] + 1
			}
			result <- partialResult
		}
		workerWG.Done()
	}()
}

func reducer(reducerWG *sync.WaitGroup, result countMap, workerResult <-chan countMap) {
	reducerWG.Add(1)
	go func() {
		for partialResult := range workerResult {
			for word, count := range partialResult {
				result[word] += count
			}
		}
		reducerWG.Done()
	}()
}

func printResult(result countMap) {
	fmt.Printf("%-10s%s\n", "Count", "Word")
	fmt.Printf("%-10s%s\n", "-----", "----")
	for word, count := range result {
		fmt.Printf("%-10v%s\n", count, word)
	}
}

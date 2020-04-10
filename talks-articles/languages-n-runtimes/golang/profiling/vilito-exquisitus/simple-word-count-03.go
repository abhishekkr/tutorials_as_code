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

// implementation: concurrent with waitgroup and mutex

var (
	cpuprofile = flag.String("cpuprofile", "", "write cpu profile to `file`")
	memprofile = flag.String("memprofile", "", "write mem profile to `file`")
)

func main() {
	flag.Parse()
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

	if len(flag.Args()) == 0 {
		fmt.Println("No files to process")
		return
	}

	result := make(map[string]int)
	resLock := new(sync.Mutex)
	waitgroup := new(sync.WaitGroup)

	start := time.Now()
	for _, filename := range flag.Args() {
		processFile(result, filename, resLock, waitgroup)
	}
	waitgroup.Wait()

	printResult(result)
	fmt.Printf("Processing took: %v\n", time.Since(start))
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

func processFile(result map[string]int, filename string, resLock *sync.Mutex, waitgroup *sync.WaitGroup) {
	waitgroup.Add(1)
	go func() {
		reader, err := os.Open(filename)
		if nil != err {
			fmt.Println(err)
			return
		}
		defer reader.Close()

		scanner := bufio.NewScanner(reader)
		scanner.Split(bufio.ScanWords)

		var word string
		for scanner.Scan() {
			word = strings.ToLower(scanner.Text())
			resLock.Lock()
			result[word] = result[word] + 1
			resLock.Unlock()
		}

		waitgroup.Done()
	}()
}

func printResult(result map[string]int) {
	fmt.Printf("%-10s%s\n", "Count", "Word")
	fmt.Printf("%-10s%s\n", "-----", "----")
	for word, count := range result {
		fmt.Printf("%-10v%s\n", count, word)
	}
}

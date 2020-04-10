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
	"time"
)

// implementation: iterative
/*
± % GO111MODULE=off go tool pprof /tmp/cpu.prof
File: simple-word-count
Type: cpu
Time: Apr 10, 2020 at 3:11am (IST)
Duration: 22.01s, Total samples = 22.18s (100.76%)
Entering interactive mode (type "help" for commands, "o" for options)
(pprof) top
Showing nodes accounting for 16720ms, 75.38% of 22180ms total
Dropped 123 nodes (cum <= 110.90ms)
Showing top 10 nodes out of 43
      flat  flat%   sum%        cum   cum%
    3410ms 15.37% 15.37%     3410ms 15.37%  unicode/utf8.DecodeRune
    2400ms 10.82% 26.19%     7060ms 31.83%  bufio.ScanWords
    2350ms 10.60% 36.79%     3890ms 17.54%  runtime.mapaccess1_faststr
    1670ms  7.53% 44.32%     2650ms 11.95%  strings.ToLower
    1450ms  6.54% 50.86%     2700ms 12.17%  runtime.mallocgc
    1300ms  5.86% 56.72%     2330ms 10.50%  runtime.mapassign_faststr
    1250ms  5.64% 62.35%     1250ms  5.64%  bufio.isSpace
    1010ms  4.55% 66.91%     1010ms  4.55%  memeqbody
     970ms  4.37% 71.28%     8490ms 38.28%  bufio.(*Scanner).Scan
     910ms  4.10% 75.38%      910ms  4.10%  aeshashbody
(pprof) png
Generating report in profile001.png
(pprof) %

± % GO111MODULE=off go tool pprof /tmp/mem.prof
File: simple-word-count
Type: inuse_space
Time: Apr 10, 2020 at 3:11am (IST)
Entering interactive mode (type "help" for commands, "o" for options)
(pprof) png
Generating report in profile002.png
(pprof) %
*/

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

	start := time.Now()
	for _, filename := range flag.Args() {
		processFile(result, filename)
	}

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

func processFile(result map[string]int, filename string) {
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
		result[word] = result[word] + 1
	}
}

func printResult(result map[string]int) {
	fmt.Printf("%-10s%s\n", "Count", "Word")
	fmt.Printf("%-10s%s\n", "-----", "----")
	for word, count := range result {
		fmt.Printf("%-10v%s\n", count, word)
	}
}

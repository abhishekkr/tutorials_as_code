package main

import (
	"bufio"
	"fmt"
	"os"
	"strings"
	"time"
)

/*
* Concurrent Design 01
{data} =>
  input-reader =(line)=> line-splitter =(words)=>
  word=counter => {output}


* Concurrent Design 02
{data} =>
  (multiple) input-reader =(line)=>            // not for magnetic drives
  line-splitter =(words)=>
  (mutex) word=counter =>                      // mutex if shared map
  {output}


* Concurrent Design 03
{data} =>
  input-reader =(line)=> (multiple) line-splitter =(words)=>
  word=counter => {output}


* Concurrent Design 04
{data} =>
  input-reader =(line)=> (multiple) line-splitter =(words)=>
  (multiple) word=counter => {output}


* Concurrent Design 05
{data} =>
  (multple) input-reader =(line)=> (multiple) line-splitter =(words)=>
  (multiple) word=counter => {output}
*/

// implementation: iterative

func main() {
	if len(os.Args) == 1 {
		//log.Error("No files to process")
		fmt.Println("No files to process")
		return
	}

	result := make(map[string]int)

	start := time.Now()
	for _, fn := range os.Args[1:] {
		processFile(result, fn)
	}

	printResult(result)
	fmt.Printf("Processing took: %v\n", time.Since(start))
}

func processFile(result map[string]int, filename string) {
	reader, err := os.Open(filename)
	if nil != err {
		//log.Warn(err)
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

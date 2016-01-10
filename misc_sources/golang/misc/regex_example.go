package main

import (
	"bufio"
	"flag"
	"fmt"
	"os"
	"regexp"
)

func grep(re, filename string) {
	regex, err := regexp.Compile(re)
	if err != nil {
		return // there was a problem with the regular expression.
	}

	fh, err := os.Open(filename)
	f := bufio.NewReader(fh)

	if err != nil {
		return // there was a problem opening the file.
	}
	defer fh.Close()

	buf := make([]byte, 1024)
	for {
		buf, _, err = f.ReadLine()
		if err != nil {
			return
		}

		s := string(buf)
		if regex.MatchString(s) {
			fmt.Printf("%s\n", string(buf))
		}
	}
}

func main() {
	flag.Parse()
	if flag.NArg() == 2 {
		grep(flag.Arg(0), flag.Arg(1))
	} else {
		fmt.Printf("Wrong number of arguments.\n")
	}
}

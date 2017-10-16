package main

/*
source: http://blog.sgmansfield.com/2015/12/goroutine-ids/
Goroutine IDs shan't be made part of logic design but only remembered if nothing else to debug works.

for 'C' version of it have a look at DaveCheney's repo
https://github.com/davecheney/junk/tree/master/id
*/

import (
	"bytes"
	"fmt"
	"runtime"
	"strconv"
)

func main() {
	fmt.Println(getGID())
}

func getGID() uint64 {
	b := make([]byte, 64)
	/*
		print a stacktrace into the buffer it’s given in textual form
	*/
	b = b[:runtime.Stack(b, false)]
	/*
		very first line in the stacktrace is the text “goroutine #### […” where #### is the actual goroutine ID]
	*/
	b = bytes.TrimPrefix(b, []byte("goroutine "))
	b = b[:bytes.IndexByte(b, ' ')]
	n, _ := strconv.ParseUint(string(b), 10, 64)
	return n
}

package main

import (
	"bytes"
	"fmt"
	"os"
	"os/exec"
	"strings"
	"time"
)

func printCommand(cmd *exec.Cmd) {
	fmt.Printf("==> Executing %s\n", strings.Join(cmd.Args, " "))
}

func printError(err error) {
	if err != nil {
		os.Stderr.WriteString(fmt.Sprintf("==> Error: %s", err.Error()))
	}
}

func printOutput(outs []byte) {
	if len(outs) > 0 {
		fmt.Printf("==> Output: %s\n", string(outs))
	}
}

func longProcess() {
	cmd := exec.Command("bash", "/tmp/ddat")
	/*/tmp/ddat
	date
	sleep 1
	date
	sleep 1
	date
	sleep 2
	date
	*/
	randomBytes := &bytes.Buffer{}
	cmd.Stdout = randomBytes

	// Start command asynchronously
	err := cmd.Start()
	printError(err)

	// Create a ticker that outputs elapsed time
	ticker := time.NewTicker(time.Second)
	go func(ticker *time.Ticker) {
		now := time.Now()
		for _ = range ticker.C {
			printOutput(
				[]byte(fmt.Sprintf("%s : %s", time.Since(now), string(randomBytes.Bytes()))),
			)
		}
	}(ticker)

	// Create a timer that will kill the process
	timer := time.NewTimer(time.Second * 4)
	go func(timer *time.Timer, ticker *time.Ticker, cmd *exec.Cmd) {
		for _ = range timer.C {
			err := cmd.Process.Signal(os.Kill)
			printError(err)
			ticker.Stop()
		}
	}(timer, ticker, cmd)

	// Only proceed once the process has finished
	cmd.Wait()
	printOutput(
		[]byte(fmt.Sprintf("%s\n%d bytes generated!", string(randomBytes.Bytes()), len(randomBytes.Bytes()))),
	)
}

func main() {
	longProcess()
}

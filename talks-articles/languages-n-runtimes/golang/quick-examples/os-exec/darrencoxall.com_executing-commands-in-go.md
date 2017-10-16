### Executing commands in Golang

[source](http://www.darrencoxall.com/golang/executing-commands-in-go/)

* 3 main reasons for system command execution
> * plain output, always expect execution and need output
> * exit codes, discard output
> * long running processes, spawn sub-processes

### Plain Output

* basics

```
func printCommand(cmd *exec.Cmd){
  fmt.Printf("==> Executing %s\n", strings.Join(cmd.Args, " "))
}

func printError(err error){
  if err != nil {
    os.Stderr.WriteString(fmt.Sprintf("==> Error: %s", err.Error()))
  }
}

func printOutput(outs []byte){
  if len(outs) > 0 {
    fmt.Printf("==> Output: %s\n", string(outs))
  }
}
```

---

* collecting output

```
cmd := exec.Command("echo", "Called from GO!")

printCommand(cmd)
output, err := cmd.CombinedOutput()
printError(err)
printOutput(output)
```

* get different output and error for finer control

```
cmd := exec.Command("go", "version")

cmdOutput := &bytes.Buffer{}  // stdout buffer
cmd.Stdout = cmdOutput  // attach buffer to command

printCommand(cmd) //execute command
err := cmd.Run()  //will wait for command to return
printError(err) //only output the commands to return
printOutput(cmdOutput.Bytes()) // => go version go1.3 darwin/amd64
```
we can similarly connect stderr and stdin streams

---

### Exit Codes

The errors occur if issue with IO or command fails.

```
cmd := exec.Command("ls", "/some/dir")

var waitStatus syscall.WaitStatus

if err := cmd.Run(); err != nil {
  printError(err)
  if exitError, ok := err.(*exec.ExitError); ok {
    waitStatus = exitError.Sys().(syscall.WaitStatus)
    exitStatus := fmt.Sprintf("%d", waitStatus.ExitStatus())
    printOutput([]byte(exitStatus))
  } else {
    waitStatus = cmd.ProcessState.Sys().(syscall.WaitStatus)
    exitStatus := fmt.Sprintf("%d", waitStatus.ExitStatus())
    printOutput([]byte(exitStatus))
  }
}
```

If Go fails to locate command in your $PATH, then it won't ever execute and thus you will have no exit code. This is why it is important to assert the type of error returned.

---

### Long Running Process

Need to happen asynchronously.

```
cmd := exec.Command("cat", "/dev/random")
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
      []byte(fmt.Sprintf("%s", time.Since(now))),
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
  []byte(fmt.Sprintf("%d bytes generated!", len(randomBytes.Bytes()))),
)

// http://play.golang.org/p/tQRk1xJOqW
```

---
---


package main

import (
  "fmt"
  "time"
  "strconv"
)

// Concurrency
// to run this $ go run $0

/*                   GoRoutines                 */

func printSquare (idx int, sleeptime int) {
  if idx % 2 == 0 {
    time.Sleep(time.Millisecond * time.Duration(sleeptime))
  }
  fmt.Println("square of ", idx, "is", idx*idx)
}

func PrintN(n int) () {
  for idx := 0; idx < n; idx++{
    go printSquare(idx, idx * 100)
  }
}

/*                    Channels                    */

// Pingr -> Recvr
func Pingr (c chan string) {
  for i := 97; i <= 122 ; i++ {
    c <- "[" + string(i) + "] P!ng"
  }
}

func Recvr (c chan string) {
  for {
    msg := <-c
    fmt.Println(msg)
    time.Sleep(time.Millisecond * 100)
  }
}

// Pingr -> Recvr <- Pongr
func Pongr (c chan string) {
  for i := 65; i <= 90 ; i++ {
    c <- "(" + string(i) + ") P0ng"
  }
}

// SendOnly -> RecvOnly
func SendOnly(c chan<- string) {
  for i := 0; i <= 10 ; i++ {
    c <- "(" + strconv.Itoa(i) + ") I can only send."
  }
}

func RecvOnly(c <-chan string) {
  for {
    msg := <-c
    fmt.Println(msg)
    time.Sleep(time.Millisecond * 100)
  }
}

// Select
func SelectChannel() {
  c1 := make(chan string)
  c2 := make(chan string)
  go func(){
    c1 <- "~~~~~~~~~"
    time.Sleep(time.Millisecond * 250)
  }()
  go func(){
    c2 <- "========="
    time.Sleep(time.Millisecond * 250)
  }()
  go func(){
    for {
      select {
      case msg1 := <-c1:
        fmt.Println(msg1)
      case msg2 := <-c2:
        fmt.Println(msg2)
      case <- time.After(time.Second * 2):
        fmt.Println("Timeout") // best place for Timeout
//    default:
//      fmt.Println("waiting...")
      }
    }
  }()
}

// Ping <-> Pong
func PingPong (msg string, ping <-chan string, pong chan<- string) {
  for reciv := range ping {
    <-time.After(2 * time.Second)
    fmt.Println(">>>>>>>", reciv)
    pong <- msg
  }
}

// Sleep using time.After
func SoJaao(n int){
  for {
    select{
      case <- time.After(time.Second * time.Duration(n)):
      return
    }
  }
}

/*                        main                    */

func main(){
  PrintN(10)
  time.Sleep(time.Second * 2)

  var c chan string = make(chan string)
  go Pingr(c)
  go Recvr(c)
  time.Sleep(time.Second * 4)

  go Pingr(c)
  go Pongr(c)
  go Recvr(c)
  time.Sleep(time.Second * 4)

  go SendOnly(c)
  go RecvOnly(c)
  time.Sleep(time.Second * 2)

  // buffered async
  var cc chan string = make(chan string, 5)
  go Pingr(cc)
  go Recvr(cc)
  time.Sleep(time.Second * 4)

  SelectChannel()
  SoJaao(1)

  ping, pong := make(chan string), make(chan string)
  go PingPong("piing", ping, pong)
  go PingPong("poong", pong, ping)
  ping <- "Ping!"

  var input string
  fmt.Println("\nEnter anytime to exit.\n", )
  fmt.Scanln(&input)
  fmt.Println("It was a formality to delay loop counts, why you entered", input)
}

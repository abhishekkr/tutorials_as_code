package main

import "C" // required to be loaded
import "fmt"

//export go_go_go
func go_go_go() {
	fmt.Println("go go go, lock 'n load")
}

func main() {
	panic("To be used as a shared object.")
}

package main

import (
	"fmt"

	jump "github.com/lithammer/go-jump-consistent-hash"
)

func main() {
	h := jump.Hash(256, 1024) // h = 520
	fmt.Println(h)

	h = jump.Hash(256, 24) // h = 520
	fmt.Println(h)

	h = jump.HashString("127.0.0.1", 8, jump.CRC64) // h = 7
	fmt.Println(h)

	h = jump.HashString("127.0.0.1", 1009, jump.CRC64) // h = 7
	fmt.Println(h)
}

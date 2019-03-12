package main

import (
	lua "github.com/Shopify/go-lua"
)

func main() {
	l := lua.NewState()
	lua.OpenLibraries(l)
	if err := lua.DoFile(l, "hello.lua"); err != nil {
		panic(err)
	}
}

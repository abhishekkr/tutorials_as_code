/* self-try */

package main

import (
	"container/ring"
	"fmt"
	"runtime"
)

var (
	Services = map[string][]string{
		"/google": {
			"http://www.google.com",
			"https://www.google.in",
		},
		"/local": {
			"http://127.0.0.1",
			"http://localhost",
		},
	}

	ServiceRings map[string]*ring.Ring
)

func main() {
	ServiceRings = make(map[string]*ring.Ring, len(Services))
	for svc, backends := range Services {
		ServiceRings[svc] = NewBackendRing(backends)
	}
	for {
		PrintBackend()
	}
}

func NewBackendRing(backends []string) *ring.Ring {
	list := ring.New(len(backends))
	for _, backend := range backends {
		list.Value = backend
		list = list.Next()
	}
	return list
}

func PrintBackend() {
	for svc, _ := range Services {
		backend := GetServiceBackend(svc)
		fmt.Println(backend)
	}
	for svc, _ := range Services {
		backend := GetServiceBackend(svc)
		fmt.Println(backend)
	}
	for svc, _ := range Services {
		backend := GetServiceBackend(svc)
		fmt.Println(backend)
	}
	for svc, _ := range Services {
		backend := GetServiceBackend(svc)
		fmt.Println(backend)
	}
	for svc, _ := range Services {
		backend := GetServiceBackend(svc)
		fmt.Println(backend)
	}
	for svc, _ := range Services {
		backend := GetServiceBackend(svc)
		fmt.Println(backend)
	}
	for svc, _ := range Services {
		backend := GetServiceBackend(svc)
		fmt.Println(backend)
	}
	PrintMemUsage()
}

func GetServiceBackend(svc string) string {
	ServiceRings[svc] = ServiceRings[svc].Next()
	return ServiceRings[svc].Value.(string)
}

func PrintMemUsage() {
	var m runtime.MemStats
	runtime.ReadMemStats(&m)
	// For info on each, see: https://golang.org/pkg/runtime/#MemStats
	fmt.Printf("Alloc = %v MiB", bToMb(m.Alloc))
	fmt.Printf("\tTotalAlloc = %v MiB", bToMb(m.TotalAlloc))
	fmt.Printf("\tHeapSys = %v MiB", bToMb(m.HeapSys))
	fmt.Printf("\tSys = %v MiB", bToMb(m.Sys))
	fmt.Printf("\tPauseTotlaNs = %v", m.PauseTotalNs)
	fmt.Printf("\tNumGC = %v\n", m.NumGC)
	fmt.Printf("\nNumGoroutine = %v\n", runtime.NumGoroutine())
}

func bToMb(b uint64) uint64 {
	return b / 1024 / 1024
}

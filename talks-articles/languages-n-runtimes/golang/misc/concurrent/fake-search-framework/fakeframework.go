package main

import (
	"fmt"
	"math/rand"
	"time"

	faker "./faker"
)

func search() {
	rand.Seed(time.Now().UnixNano())
	start := time.Now()
	//results := faker.GoogleSequential("golang")
	//results := faker.GoogleMinimalTime("golang")
	//results := faker.GoogleAvoidSlow("golang")
	results := faker.GoogleReplicaAndLatency("golang")
	elapsed := time.Since(start)
	fmt.Println(results)
	fmt.Println(elapsed)
}

func searchReplica() {
	rand.Seed(time.Now().UnixNano())
	start := time.Now()
	results := faker.GoogleFirst("golang",
		faker.FakeSearch("replica#1"),
		faker.FakeSearch("replica#2"))
	elapsed := time.Since(start)
	fmt.Println(results)
	fmt.Println(elapsed)
}

func main() {
	search()
	searchReplica()
}

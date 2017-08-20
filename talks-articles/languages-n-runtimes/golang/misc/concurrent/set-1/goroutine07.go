package main

import (
	"fmt"
	"runtime"
	"strings"
	"time"
)

type pipe chan string
type Engines map[string]pipe

func ChurnTwitter(dat string) string {
	return strings.Fields(dat)[0]
}

func ChurnFacebook(dat string) string {
	return strings.Fields(dat)[1]
}

func FanInDataStreams(twitter, facebook <-pipe) pipe {
	out := make(pipe)
	go func() {
		for {
			select {
			case dat := <-twitter:
				out <- ChurnTwitter(dat)
			case dat := <-facebook:
				out <- ChurnFacebook(dat)
			}
		}
	}()
	return out
}

func main() {
	var engines Engines
	engines = make(Engines)

	socialNetworks := []string{"twitter", "facebook"}


	runtime.GOMAXPROCS(10)
	for _, socialNet := range socialNetworks {
		engines[socialNet] := make(pipe, 5)
	}

	dataminer := FanInDataStreams(engines["twitter"], engines["facebook"])

	engines["twitter"] <- "chirp anythin"
	engines["facebook"] <- "anythin like"
	engines["twitter"] <- "DM whatever"
	engines["twitter"] <- "follow alike"
	engines["facebook"] <- "try share"

	for {
		select {
		case nfo := <-dataminer:
			fmt.Println("~~", nfo)
		case <-time.After(time.Second * 2):
			fmt.Println("sayonara")
			return
		}
	}

	runtime.Gosched()
}

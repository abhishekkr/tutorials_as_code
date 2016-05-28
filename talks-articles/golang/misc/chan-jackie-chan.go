package main

import (
	"flag"
	"fmt"
	"strconv"
	"time"
)

type Jackie struct {
	uuid string
}

type JackieChan struct {
	jackie chan Jackie
}

var (
	jackieChan        *JackieChan
	updateWorkerCount = 10
	flagDelay         = flag.Int64("delay", 2, "seconds delay introduced to accumulate channel queue")
	flagBuffer        = flag.Int("buffer", 2, "buffer for channel queue")
)

func findJackieChan(channelBuffer int) {
	jackieChan = &JackieChan{
		jackie: make(chan Jackie, channelBuffer),
	}
}

func hireJackieChan() chan Jackie {
	return jackieChan.jackie
}

func shootScenes(sceneId string) {
	chanCount := len(hireJackieChan())

	fmt.Printf("[ remaining Jackie Chan scenes %d when id#%v ]\n", chanCount, sceneId)
	hireJackieChan() <- Jackie{uuid: sceneId}
}

func editMovie() {
	for i := 0; i < updateWorkerCount; i++ {
		go func(queue chan Jackie, chanId int) {
			for sceneId := range queue {
				fmt.Printf("Jackie Chan shooting for Scene#%s at Channel%v\n", sceneId, chanId)
			}
		}(hireJackieChan(), i)

	}
}

func accumulatedScenes(delaySecond int64) {
	for sceneId := 0; sceneId < 10; sceneId++ {
		go shootScenes(strconv.Itoa(sceneId))
	}

	time.Sleep(time.Duration(delaySecond) * time.Second)
	chanCount := len(jackieChan.jackie)
	fmt.Printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>> %v\n", chanCount)

	for sceneId := 11; sceneId < 20; sceneId++ {
		go shootScenes(strconv.Itoa(sceneId))
	}

	editMovie()
}

func main() {
	flag.Parse()
	findJackieChan(*flagBuffer)
	accumulatedScenes(*flagDelay)
	time.Sleep(2 * time.Second)
}

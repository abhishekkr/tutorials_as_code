package main

import (
	"context"
	"fmt"
	"io"
	"log"

	"google.golang.org/grpc"

	"github.com/abhishekkr/gol/golenv"

	logr "../logr"
)

var (
	LOGR_AT = golenv.OverrideIfEnv("LOGR_AT", "127.0.0.1:1234")
)

func createLogr(client logr.LogrClient, l *logr.LogrRequest) {
	resp, err := client.CreateLog(context.Background(), l)

	if err != nil || !resp.Success {
		log.Printf("create logr failed\nerr: %q\nresponse: %q", err.Error(), resp.Msg)
	}
}

func getLogrs(client logr.LogrClient, filtr *logr.LogrFilter) {
	stream, err := client.GetLogs(context.Background(), filtr)
	if err != nil {
		log.Println("error on get logr: ", err.Error())
		return
	}

	for {
		l, err := stream.Recv()
		if err == io.EOF {
			break
		}
		if err != nil {
			log.Printf("%v.GetLogs(_) = _, %v", client, err)
		}
		fmt.Println("Logr: ", l)
	}
}

func pushSomeDummyLogs(client logr.LogrClient) {
	xlog := &logr.LogrRequest{
		App:     "myapp",
		Logline: "something happened in this app that need be recorded",
		Tags: []*logr.LogrRequest_LogrTags{
			&logr.LogrRequest_LogrTags{
				Tag: "app",
			},
		},
	}
	createLogr(client, xlog)

	ylog := &logr.LogrRequest{
		App:     "urapp",
		Logline: "nothing happened here and still got recorded",
		Tags: []*logr.LogrRequest_LogrTags{
			&logr.LogrRequest_LogrTags{
				Tag: "app",
			},
			&logr.LogrRequest_LogrTags{
				Tag: "error",
			},
		},
	}
	createLogr(client, ylog)
}

func main() {
	conn, err := grpc.Dial(LOGR_AT, grpc.WithInsecure())
	if err != nil {
		log.Fatalln("did not connect: ", err.Error())
		return
	}
	defer conn.Close()
	log.Println("starting client...")

	client := logr.NewLogrClient(conn)
	pushSomeDummyLogs(client)

	log.Println("--- all")
	filterx := &logr.LogrFilter{App: ""}
	getLogrs(client, filterx)

	log.Println("--- myapp")
	filtery := &logr.LogrFilter{App: "myapp"}
	getLogrs(client, filtery)
}

package main

import (
	"context"
	"fmt"
	"log"
	"net"

	"google.golang.org/grpc"

	"github.com/abhishekkr/gol/golenv"

	logr "../logr"
)

var (
	LOGR_PORT = golenv.OverrideIfEnv("LOGR_PORT", ":1234")
)

type Logr struct {
	savedLogs []*logr.LogrRequest
}

func main() {
	conn, err := net.Listen("tcp", LOGR_PORT)
	if err != nil {
		log.Fatalln("failed to bind at", LOGR_PORT)
		return
	}

	log.Println("starting server...")
	svr := grpc.NewServer()
	logr.RegisterLogrServer(svr, &Logr{})
	svr.Serve(conn)
}

func (l *Logr) CreateLog(c context.Context, input *logr.LogrRequest) (*logr.LogrResponse, error) {
	l.savedLogs = append(l.savedLogs, input)
	return &logr.LogrResponse{
		Success: true,
		Msg:     fmt.Sprintf("success, total logs: %d", len(l.savedLogs)),
	}, nil
}

func (l *Logr) GetLogs(filtr *logr.LogrFilter, stream logr.Logr_GetLogsServer) error {
	for _, ilog := range l.savedLogs {
		if !matchLogr(ilog, filtr) {
			continue
		}

		err := stream.Send(ilog)
		if err != nil {
			return err
		}
	}
	return nil
}

func matchLogr(ilog *logr.LogrRequest, filtr *logr.LogrFilter) bool {
	if filtr.App == "" && len(filtr.Tags) == 0 {
		return true
	}

	if filtr.App != "" && filtr.App != ilog.App {
		return false
	}

	for _, xtag := range filtr.Tags {
		if xtag.Tag == "" {
			continue
		}
		for _, ytag := range ilog.Tags {
			if xtag.Tag == ytag.Tag {
				break
			}
		}
		return false
	}

	return true
}

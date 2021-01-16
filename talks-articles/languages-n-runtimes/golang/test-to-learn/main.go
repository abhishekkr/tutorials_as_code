package main

import (
	"fmt"
	"net/http"

	health "github.com/abhishekkr/tutorials_as_code/talks-articles/languages-n-runtimes/golang/test-to-learn/health"
)

func isUrlAvailable(svc health.GenericService) string {
	if svc.Available() {
		return "available"
	}
	return "failed"
}

func main() {
	url := "http://127.0.0.1:9090"
	svc := health.Service{Client: &http.Client{}, URL: url}
	fmt.Printf("status for %v: %v\n", svc.Info(), isUrlAvailable(svc))
}

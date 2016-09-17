package main

import (
	"flag"
	"log"
	"net/http"
)

func main() {
	flag.Parse()
	serve_dir := *flag.String("serve", "", "will serve current dir if empty")
	serve_port := *flag.String("port", ":8888", "port need to be proivded with colon, as lsof")

	log.Println("Listening at port", serve_port)
	log.Fatal(
		http.ListenAndServe(
			serve_port,
			http.FileServer(
				http.Dir(serve_dir))))

}

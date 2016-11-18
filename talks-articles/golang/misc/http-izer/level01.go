/*
Using Gorilla Mux with paths as params.
*/
package main

import (
	"fmt"
	"net/http"

	"github.com/gorilla/mux"
)

func RootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "<h1>Hooligans</h1>")
}

func STFUHandler(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	fmt.Fprintf(w, "<h1>Hooligans!</h1><h3>%s %s</h3>", params["st"], params["fu"])
}

func main() {
	fmt.Println("go browse: localhost:8888")
	fmt.Println("Example: `curl http://localhost:8888/$USER/$GROUP`")
	gmux := mux.NewRouter()
	gmux.HandleFunc("/", RootHandler)
	gmux.HandleFunc("/{st}/{fu}", STFUHandler)
	http.ListenAndServe(":8888", gmux)
}

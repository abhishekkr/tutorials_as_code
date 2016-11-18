/*
Vanilla golang http
*/
package main

import (
	"fmt"
	"net/http"
)

func RootHandler(w http.ResponseWriter, r *http.Request) {
	fmt.Fprintln(w, "<h1>Hooligans</h1>")
}

func STFUHandler(w http.ResponseWriter, r *http.Request) {
	f := r.FormValue("what")
	fmt.Fprintf(w, "<h1>Hooligans!</h1><h3>Shut the %s up.</h3>", f)
}

func main() {
	fmt.Println("go browse: localhost:8888")
	fmt.Println("Example: `curl http://localhost:8888/stfu?what=koolaid`")
	http.HandleFunc("/", RootHandler)
	http.HandleFunc("/stfu", STFUHandler)
	http.ListenAndServe(":8888", nil)
}

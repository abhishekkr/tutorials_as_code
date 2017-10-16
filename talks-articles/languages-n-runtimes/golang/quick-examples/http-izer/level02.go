/*
Using Gorilla Mux with templates and go-structs.
*/
package main

import (
	"fmt"
	"html/template"
	"net/http"

	"github.com/gorilla/mux"
)

var templates *template.Template

type STFU struct {
	ST string
	FU string
}

type Blimey struct {
	Eh string
}

func RootHandler(w http.ResponseWriter, r *http.Request) {
	templates.ExecuteTemplate(w, "hooligans.html", nil)
}

//without file templates
func STFUHandler(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)

	t := template.New("stfu")
	stfu := STFU{
		params["st"],
		params["fu"],
	}
	t.Parse(`<h1>Hooligans!</h1><h3>{{.ST}} {{.FU}}.</h3>`)
	t.Execute(w, stfu)
}

//with file templates
func BlimeyHandler(w http.ResponseWriter, r *http.Request) {
	params := mux.Vars(r)
	blimey := Blimey{Eh: params["huh"]}
	templates.ExecuteTemplate(w, "blimey.html", blimey)
}

func init() {
	templates = template.Must(template.ParseFiles(
		"./templates/blimey.html",
		"./templates/hooligans.html"))
}

func main() {
	fmt.Println("go browse: localhost:8888")
	fmt.Println("Example: `curl http://localhost:8888/$USER/$GROUP`")
	fmt.Println("Example: `curl http://localhost:8888/blimey/$USER`")
	gmux := mux.NewRouter()
	gmux.HandleFunc("/", RootHandler)
	gmux.HandleFunc("/blimey/{huh}", BlimeyHandler) // need to come before more generic handler
	gmux.HandleFunc("/{st}/{fu}", STFUHandler)
	http.ListenAndServe(":8888", gmux)
}

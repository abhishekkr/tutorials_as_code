package main

import (
	"encoding/json"
	"fmt"
	"log"
	"net/http"
	"strconv"
	"time"

	"github.com/ant0ine/go-json-rest/rest"
)

type Paste struct {
	Uuid      uint32 `json:UUID`
	Timestamp time.Time
}

type Clipboard struct {
	Clips map[uint32]Paste
}

var clipboard = &Clipboard{
	Clips: make(map[uint32]Paste),
}

func StoreGet(parsedId uint32) (paste Paste, ok bool) {
	for _, v := range clipboard.Clips {
		if v.Uuid == parsedId {
			fmt.Println(v.Uuid, "was pinged at", v.Timestamp)
			paste.Uuid, paste.Timestamp = v.Uuid, v.Timestamp
			ok = true
			return
		}
	}
	return
}

func StoreCreate(parsedId uint32) {
	paste := Paste{}
	paste.Uuid = parsedId
	paste.Timestamp = time.Now()
	clipboard.Clips[parsedId] = paste
}

func GetPaste(w rest.ResponseWriter, req *rest.Request) {
	parsedId, err := strconv.ParseUint(req.PathParam("id"), 10, 32)
	if err != nil {
		rest.NotFound(w, req)
		return
	}
	if paste, ok := StoreGet(uint32(parsedId)); ok {
		w.WriteJson(paste)
		return
	}
	rest.Error(w, "not found", http.StatusNotFound)
}

func PostPaste(w rest.ResponseWriter, req *rest.Request) {
	decoder := json.NewDecoder(req.Body)
	var paste Paste
	err := decoder.Decode(&paste)
	if err != nil {
		panic(err)
	}

	if err != nil {
		rest.NotFound(w, req)
		return
	}
	StoreCreate(uint32(paste.Uuid))
	w.WriteJson(clipboard)
}

func GetAll(w rest.ResponseWriter, req *rest.Request) {
	w.WriteJson(clipboard)
}

func MakeApiSimple() http.Handler {
	api := rest.NewApi()
	//  logging, json indenting, stacktrace on panic
	api.Use(rest.DefaultDevStack...)

	router, err := rest.MakeRouter(
		&rest.Route{"GET", "/pastes/:id", GetPaste},
		&rest.Route{"POST", "/pastes", PostPaste},
		&rest.Route{"GET", "/pastes", GetAll},
	)

	if err != nil {
		log.Fatal(err)
	}

	api.SetApp(router)
	return api.MakeHandler()
}

func main() {
	apiHandler := MakeApiSimple()
	log.Fatal(http.ListenAndServe(":8080", apiHandler))
}

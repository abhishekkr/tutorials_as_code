
## Writing JSON REST APIs in GO
(30/Oct/2015 Zurich Gopher Meetup)

> Why
> * Performance : [Parse](http://blog.parse.com/learn/how-we-moved-our-api-from-ruby-to-go-and-saved-our-sanity/) and [Repustate](https://blog.repustate.com/migrating-entire-api-go-python)
> * Maintainability

---

* a quick usage with Type Safety checks

```golang
func StringInterfaceMap() {
	raw_data := []byte(`{"msg": "Hello Go!", "id": 12345}`)
	decoded := map[string]interface{}{}
	err := json.Unmarshal(raw_data, &decoded)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(decoded["msg"].(string), int(decoded["id"].(float64)))
}
```

> output
```
Hello Go! 12345
```
---

* un-exported structs, no value mapped and no error

```
type Msg1 struct {
	msg string
	id  int64
}

func UnexportedStruct() {
	raw_data := []byte(`{"msg": "Hello Go!", "id": 12345}`)
	decoded := Msg1{}
	err := json.Unmarshal(raw_data, &decoded)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(decoded.msg, decoded.id)
}
```

> output
```
Press ENTER or type command to continue
 0
```
---

* exported simple struct, but on re-encoding get Capitalized entity names

```
type Msg1 struct {
	Msg string
	Id  int64
}

func UnexportedStruct() {
	raw_data := []byte(`{"msg": "Hello Go!", "id": 12345}`)
	decoded := Msg1{}
	err := json.Unmarshal(raw_data, &decoded)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(decoded.Msg, decoded.Id)

	encoded, _ := json.Marshal(&decoded)
	fmt.Println(string(encoded))
}
```

> output
```
Hello Go! 12345
{"Msg":"Hello Go!","Id":12345}
```

---

* resolution for this all, exported Struct but support proper re-encoding

```
type Msg1 struct {
	Msg string `json:"msg"`
	Id  int64  `json:"id"`
}

func UnexportedStruct() {
	raw_data := []byte(`{"msg": "Hello Go!", "id": 12345}`)
	decoded := Msg1{}
	err := json.Unmarshal(raw_data, &decoded)

	if err != nil {
		log.Fatal(err)
	}

	fmt.Println(decoded.Msg, decoded.Id)

	encoded, _ := json.Marshal(&decoded)
	fmt.Println(string(encoded))
}
```

> output
```
Hello Go! 12345
{"msg":"Hello Go!","id":12345}
```

---

* Be aware of tags
* Pass a reference to Unmarshal
* Be aware of nil reference types

---

#### Go-JSON-REST by Antoine Imbert
[https://github.com/ant0ine/go-json-rest](https://github.com/ant0ine/go-json-rest)

Features:
* Improved router and handler interface for REST functionality
* Middleware Support

[Examples](https://github.com/ant0ine/go-json-rest-examples)

```
go get github.com/ant0ine/go-json-rest
```

---

Setting up Go-Json-Rest

```
func GetPaste(w rest.ResponseWriter, req *rest.Request)  {}
func PostPaste(w rest.ResponseWriter, req *rest.Request) {}
func GetAll(w rest.ResponseWriter, req *rest.Request)    {}

func MakeApiSimple() http.Handler {
	api := rest.NewApi()
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
```

---

* adding middleware to app
```
 api.Use(&package.MyMiddleware{/*OPTIONS*/})
```

Example of adding JWT Auth, authentication handled automatically
```
jwtMiddleware := &jwt.JWTMiddleware{
  Key: []byte("super-secret-key"),
  Realm: "Pastebin",
  Timeout: time.Hour,
  MaxRefresh: time.Hour * 24,
  Authenticator: authUser,
}

api.Use(&rest.IfMiddleware{
  Condition: func(request *rest.Request) bool {
    return request.URL.Path != "/login" && request.Method == "POST"
  },
  IfTrue: jwtMiddleware,
})
```

---

* 'x/net/context'

more from sources on context as [text](https://blog.golang.org/context), [video](https://vimeo.com/115309491)

A Context carries a deadline, a cancellation signal, and other values across API boundaries.

* city

* weather
> have http contexts served over channels in different goroutine

---


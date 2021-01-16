package health

import (
	"log"
	"net/http"
)

type GenericService interface {
	Available() bool
	Info() string
}

type Service struct {
	Client *http.Client
	URL    string
}

func (s Service) Info() string {
	return s.URL
}

func (s Service) Available() bool {
	response, err := http.Get(s.URL)
	defer response.Body.Close()
	if err != nil {
		log.Println(err)
		return false
	}
	if response.StatusCode > 399 {
		log.Println(err)
		return false
	}
	return true
}

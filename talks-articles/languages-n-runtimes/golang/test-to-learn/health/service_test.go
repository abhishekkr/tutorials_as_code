package health

import (
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"net/http/httptest"
	"testing"
)

var (
	ValidPaths = []string{
		"/path",
		"/path/x",
		"/path/x/y",
	}
	InvalidPaths = []string{
		"/~",
		"/404",
	}
)

func init() {
	log.SetOutput(ioutil.Discard)
}

func newTestServer(code int) *httptest.Server {
	return httptest.NewServer(
		http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
			w.WriteHeader(code)
			fmt.Fprintln(w, "whatever.")
		}),
	)
}

func TestServiceAvailableTrue(t *testing.T) {
	validServer := newTestServer(http.StatusOK)
	defer validServer.Close()

	for _, urlPath := range ValidPaths {
		validClient := Service{Client: validServer.Client(), URL: validServer.URL + urlPath}
		if validClient.Available() == false {
			t.Errorf("httpStatus failed")
		}
	}
}

func TestServiceAvailableFalse(t *testing.T) {
	invalidServer := newTestServer(http.StatusBadRequest)
	defer invalidServer.Close()

	for _, urlPath := range InvalidPaths {
		invalidClient := Service{Client: invalidServer.Client(), URL: invalidServer.URL + urlPath}
		if invalidClient.Available() {
			t.Errorf("httpStatus failed")
		}
	}
}

func BenchmarkServiceAvailableTrue(b *testing.B) {
	validServer := newTestServer(http.StatusOK)
	defer validServer.Close()
	validClient := Service{Client: validServer.Client(), URL: validServer.URL}

	for i := 0; i < b.N; i++ {
		validClient.Available()
	}
}

func BenchmarkServiceAvailableFalse(b *testing.B) {
	invalidServer := newTestServer(http.StatusBadRequest)
	defer invalidServer.Close()
	invalidClient := Service{Client: invalidServer.Client(), URL: invalidServer.URL}

	for i := 0; i < b.N; i++ {
		invalidClient.Available()
	}
}

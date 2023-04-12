package main

import (
	"fmt"
	"sync"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/olahol/melody"
)

var uuid int

type SessionMgr struct {
	mux      sync.RWMutex
	sessions map[interface{}]*melody.Session
}

func (mgr *SessionMgr) Get(id interface{}) (*melody.Session, bool) {
	mgr.mux.RLock()
	defer mgr.mux.RUnlock()
	sess, ok := mgr.sessions[id]
	return sess, ok
}

func (mgr *SessionMgr) Set(id interface{}, sess *melody.Session) {
	mgr.mux.Lock()
	defer mgr.mux.Unlock()
	mgr.sessions[id] = sess
}

func (mgr *SessionMgr) Delete(id interface{}) {
	mgr.mux.Lock()
	defer mgr.mux.Unlock()
	delete(mgr.sessions, id)
}

func (mgr *SessionMgr) ForEach(filter func(sess *melody.Session) bool, handler func(sess *melody.Session)) {
	mgr.mux.RLock()
	defer mgr.mux.RUnlock()
	for _, sess := range mgr.sessions {
		if filter(sess) {
			handler(sess)
		}
	}
}

var (
	sessionMgr = SessionMgr{
		sessions: map[interface{}]*melody.Session{},
	}
)

func getIDFromSession(sess *melody.Session) string {
	if sess.Keys["uuid"] == nil {
		uuid++
		return fmt.Sprintf("%v", uuid)
	}
	return sess.Keys["uuid"].(string)
}

func main() {
	r := gin.Default()
	m := melody.New()

	m.HandleConnect(func(sess *melody.Session) {
		id := getIDFromSession(sess)
		fmt.Println("WS joined", id)
		sessionMgr.Set(id, sess)
	})

	m.HandleDisconnect(func(sess *melody.Session) {
		id := getIDFromSession(sess)
		fmt.Println("WS left", id)
		sessionMgr.Delete(id)
	})

	r.GET("/ws", func(c *gin.Context) {
		m.HandleRequest(c.Writer, c.Request)
	})

	m.HandleMessage(func(s *melody.Session, msg []byte) {
		s.Write(msg)
	})

	go ping("1", "ping", 15)
	go ping("2", "p0ng", 20)
	go ping("3", "p1ng", 20)
	go ping("4", "p2ng", 20)
	go ping("5", "p3ng", 20)

	r.Run(":5000")
}

func ping(id, msg string, delay int) {
	time.Sleep(time.Duration(delay) * time.Second)
	if s, ok := sessionMgr.Get(id); ok {
		s.Write([]byte(msg))
	}
}

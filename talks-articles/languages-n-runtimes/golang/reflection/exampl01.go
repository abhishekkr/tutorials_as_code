package main

/*
* https://scene-si.org/2017/12/21/introduction-to-reflection/
 */

import (
	"log"
	"reflect"
	"strings"
	"time"
)

type Message struct {
	ID         uint64    `db:"id"`
	Channel    string    `db:"channel"`
	UserName   string    `db:"user_name"`
	UserID     string    `db:"user_id"`
	UserAvatar string    `db:"user_avatar"`
	Message    string    `db:"message"`
	RawMessage string    `db:"message_raw"`
	MessageID  string    `db:"message_id"`
	Stamp      time.Time `db:"stamp"`
}

func main() {
	msg := Message{
		ID:       1234,
		UserName: "DevAnand",
		UserID:   "Dev",
	}
	Reflekt(msg)
	genInsertStatement("chathouse", msg)
}

func Reflekt(obj interface{}) {
	obj_value := reflect.ValueOf(obj)
	if obj_value.Kind() == reflect.Ptr {
		obj_value = obj_value.Elem()
	}
	log.Printf("sent instance got %d fields", obj_value.NumField())

	obj_fields := make([]struct {
		Name  string
		Tag   string
		Value interface{}
	}, obj_value.NumField())

	for i := 0; i < len(obj_fields); i++ {
		fieldValue := obj_value.Field(i)
		fieldType := obj_value.Type().Field(i)
		obj_fields[i].Name = fieldType.Name
		obj_fields[i].Value = fieldValue.Interface()
		obj_fields[i].Tag = fieldType.Tag.Get("db")
		log.Printf("%q", obj_fields[i])
	}

}

func genInsertStatement(table string, data interface{}) string {
	message_value := reflect.ValueOf(data)
	if message_value.Kind() == reflect.Ptr {
		message_value = message_value.Elem()
	}

	message_fields := make([]string, message_value.NumField())

	for i := 0; i < len(message_fields); i++ {
		fieldType := message_value.Type().Field(i)
		message_fields[i] = fieldType.Tag.Get("db")
	}

	sql := "insert into " + table + " set"
	for _, tagFull := range message_fields {
		if tagFull != "" && tagFull != "-" {
			tag := strings.Split(tagFull, ",")
			sql = sql + " " + tag[0] + "=:" + tag[0] + ","
		}
	}
	log.Println(sql[:len(sql)-1])
	return sql[:len(sql)-1]
}

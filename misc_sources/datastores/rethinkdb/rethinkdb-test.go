package main

import (
	"fmt"
	"flag"
	"log"

	r "github.com/dancannon/gorethink"
)

var (
	flagDbUri = flag.String("uri", "localhost:28015", "what db URI to connect to")
	flagDbName = flag.String("db", "dbtalk", "the database name you wanna interact with")
	flagTableName = flag.String("table", "rethinkdb", "the table name you wanna interact with")

	flagAction = flag.String("action", "", "action that you want to take at rethinkdb")
)

func getDbSession(dbUri, dbName string) (session *r.Session) {
	session, err := r.Connect(r.ConnectOpts{
		Address: dbUri,
		Database: dbName,
	})
	if err != nil {
		log.Fatalln(err.Error())
	}
	return
}

func createDb(session *r.Session, dbName string) {
	err := r.DBCreate(dbName).Exec(session)
	if err != nil {
		fmt.Printf("%v", err)
		log.Fatalln(err)
		return
	}

	fmt.Printf("%v DB created.\n", dbName)
}

func dropDb(session *r.Session, dbName string) {
	resp, err := r.DBDrop(dbName).RunWrite(session)
	if err != nil {
		log.Fatalln(err)
		return
	}

	fmt.Printf("%v: %d DB dropped, %d tables dropped.\n", dbName, resp.DBsDropped, resp.TablesDropped)
}

func createTable(session *r.Session, dbName string, tableName string) {
	err := r.DB(dbName).TableCreate(tableName).Exec(session)
	if err != nil {
		log.Fatalln(err)
		return
	}

	fmt.Printf("Table %v created under %v DB.\n", tableName, dbName)
}

func dropTable(session *r.Session, dbName string, tableName string) {
	_, err := r.DB(dbName).TableDrop(tableName).Run(session)
	if err != nil {
		log.Fatalln(err)
		return
	}

	fmt.Printf("%v.%v table dropped.\n", dbName, tableName)
}

func main(){
	flag.Parse()
	fmt.Println("VLAD: RethinkDB connected!")
	session := getDbSession(*flagDbUri, *flagDbName)
	defer session.Close()

	switch *flagAction {
		case "db":
			createDb(session, *flagDbName)
		case "dropdb":
			dropDb(session, *flagDbName)
		case "table":
			createTable(session, *flagDbName, *flagTableName)
		case "droptable":
			dropTable(session, *flagDbName, *flagTableName)
	}
}

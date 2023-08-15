package main

import (
	"encoding/json"
	"fmt"

	_ "github.com/go-sql-driver/mysql"
	"github.com/jmoiron/sqlx"
)

func main() {
	//Simple Employee JSON object which we will parse
	empA := `{
			"id": 1,
			"name": "Mr. Boss",
			"department": "",
			"designation": "Director",
			"city": "Mumbai",
			"state": "Maharashtra",
			"country": "India"
		}`
	empB := `{
			"id": 11,
			"name": "Irshad",
			"department": "IT",
			"designation": "Product Manager",
			"city": "Mumbai",
			"state": "Maharashtra",
			"country": "India"
		}`
	empC := `{
			"id": 12,
			"name": "Pankaj",
			"department": "IT",
			"designation": "Team Lead",
			"city": "Pune",
			"state": "Maharashtra",
			"country": "India"
		}`

	db, err := sqlx.Connect("mysql", "test:test@(localhost:3306)/testdb")
	if err != nil {
		panic(err)
	}
	xschema(db)
	xjson(db, empA)
	xjson(db, empB)
	xjson(db, empC)
}

func xjson(db *sqlx.DB, empArray string) {
	// Declared an empty interface of type Array
	var results map[string]interface{}

	// Unmarshal or Decode the JSON to the interface.
	err := json.Unmarshal([]byte(empArray), &results)

	fmt.Println(err)
	fmt.Println(results["name"])
	keysize := len(results)
	fields := ""
	ph := ""
	values := make([]interface{}, keysize)
	idx := 0
	for key, result := range results {
		values[idx] = result
		idx++
		fmt.Printf("%v : %v\n", key, result)
		if idx < keysize {
			fields += key + ", "
			ph += "?, "
		} else {
			fields += key
			ph += "?"
		}
	}
	sqlstr := fmt.Sprintf("INSERT INTO xtable (%s) VALUES (%s)", fields, ph)
	db.MustExec(sqlstr, values...)
}

func xschema(db *sqlx.DB) {
	schema := `CREATE TABLE xtable (
		id integer,
		name varchar(50),
		department varchar(50),
		designation varchar(50),
		city varchar(50),
		state varchar(50),
		country varchar(50)
	);`

	// execute a query on the server
	result, err := db.Exec(schema)
	if err != nil {
		panic(err)
	}
	fmt.Println(result)
}

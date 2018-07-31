/* https://github.com/jdkato/prose */

package main

import (
	"fmt"
	"log"

	prose "gopkg.in/jdkato/prose.v2"
)

func readProse(s string) {
	doc, err := prose.NewDocument(s) //, prose.WithExtraction(false))
	if err != nil {
		log.Fatal(err)
	}
	fmt.Println("*******************")
	fmt.Println(s)
	fmt.Println("******************* tokens")
	for _, tok := range doc.Tokens() {
		fmt.Println(tok.Text, tok.Tag, tok.Label)
	}
	fmt.Println("******************* named entities")
	for _, ent := range doc.Entities() {
		fmt.Println(ent.Text, ent.Label)
	}
	fmt.Println("******************* sentence")
	for _, sent := range doc.Sentences() {
		fmt.Println(sent.Text)
	}

	fmt.Println("*******************")
	fmt.Println("*******************")
}

func main() {
	readProse("@jdkato, go to http://example.com thanks :).")
	readProse("Go is an open-source programming language created at Google.")
}

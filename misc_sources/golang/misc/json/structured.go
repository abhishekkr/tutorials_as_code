package main

import (
	"encoding/json"
	"fmt"
	"io/ioutil"
)

type EngineDestination struct {
	DestinationIP    string `json:"destination_ip"`
	DestinationPorts []int  `json:"destination_ports"`
	SplitterMode     string `json:"mode"`
	SplitterType     string `json:"type"`
	SplitterPattern  string `json:"pattern"`
}

type EngineDetail struct {
	SourceIP     string              `json:"source_ip"`
	SourcePorts  []int               `json:"source_ports"`
	Destinations []EngineDestination `json:"destinations"`
}

type EngineCollection struct {
	Engines []EngineDetail
}

func main() {
	structured_json := "structured.json"
	jsonBytes, fileErr := ioutil.ReadFile(structured_json)
	if fileErr != nil {
		panic(fmt.Sprintf("ERROR: failed reading file %s", structured_json))
	}

	var engines EngineCollection
	err := json.Unmarshal(jsonBytes, &engines.Engines)
	if err != nil {
		panic("ERROR: failed unmarshalling json from file")
	}

	fmt.Println(engines)
}

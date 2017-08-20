package main

/*
Task: Summarize the hudson set-up
Author: abhishekkr <abhikumar163@gmail.com, @abionic>
*/

import (
	"encoding/xml"
	"fmt"
	"io"
	"os"
	"path/filepath"
)

type HudsonConfig struct {
	XMLName  xml.Name `xml:"hudson"`
	JobNames []string `xml:"views>listView>jobNames>string"`
}

func ReadHudsonConfig(reader io.Reader) (HudsonConfig, error) {
	xmlHudsonConfig := HudsonConfig{}
	decoder := xml.NewDecoder(reader)

	if err := decoder.Decode(&xmlHudsonConfig); err != nil {
		return HudsonConfig{}, err
	}

	return xmlHudsonConfig, nil
}

func FileReader(file_path string) *os.File {
	var file *os.File
	defer func() {
		if file != nil {
			file.Close()
		}
	}()

	absolute_file_path, err := filepath.Abs(file_path)
	if err != nil {
		panic(err.Error())
	}

	filereader, err := os.Open(absolute_file_path)
	if err != nil {
		panic(err.Error())
	}
	return filereader
}

func main() {
	filereader := FileReader("config.xml")
	xmlHudsonConfig, err := ReadHudsonConfig(filereader)
	if err != nil {
		panic(err.Error())
	}

	fmt.Println(xmlHudsonConfig.XMLName)
	for _, job := range xmlHudsonConfig.JobNames {
		fmt.Println(job)
	}
}

package main

import (
	"crypto/md5"
	"fmt"
	"io"
	"log"

	"github.com/peterbourgon/diskv"
)

const transformBlockSize = 2 // grouping of chars per directory depth

func blockTransform(s string) []string {
	var (
		sliceSize = len(s) / transformBlockSize
		pathSlice = make([]string, sliceSize)
	)
	for i := 0; i < sliceSize; i++ {
		from, to := i*transformBlockSize, (i*transformBlockSize)+transformBlockSize
		pathSlice[i] = s[from:to]
	}
	return pathSlice
}

func allKeys(kv *diskv.Diskv) {
	var keyCount int
	for range kv.Keys(nil) {
		keyCount++
	}
	log.Println(keyCount)

	keyIndex := 0
	keyList := make([]string, keyCount)
	for key := range kv.Keys(nil) {
		keyList[keyIndex] = key
		keyIndex += 1
	}
	fmt.Println(keyList)
}

func printDetails(kv *diskv.Diskv) {
	var keyCount int
	for key := range kv.Keys(nil) {
		val, err := kv.Read(key)
		if err != nil {
			panic(fmt.Sprintf("key %s had no value", key))
		}
		fmt.Printf("%s: %s\n", key, val)
		keyCount++
	}
	fmt.Printf("%d total keys\n", keyCount)
}

func main() {
	d := diskv.New(diskv.Options{
		BasePath:     "data",
		Transform:    blockTransform,
		CacheSizeMax: 1024 * 1024, // 1MB
	})

	for _, valueStr := range []string{
		"Salutation: 'Eeparrei!', or 'Epahhey, Oia!'",
		"Consecrated day: Wednesday",
		"Colors: Brown, red, pink, and white",
		"Symbols: eruquerê, a ritual object; or a copper sword",
		"Prohibitions: pumpkin, stingray, and mutton",
		"Food: acarajé[7]",
	} {
		d.Write(md5sum(valueStr), []byte(valueStr))
	}

	allKeys(d)
	printDetails(d)

	// d.EraseAll() // leave it commented out to see how data is kept on disk
}

func md5sum(s string) string {
	h := md5.New()
	io.WriteString(h, s)
	return fmt.Sprintf("%x", h.Sum(nil))
}

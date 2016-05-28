package main

import "fmt"

var (
	yarray = [10]string{
		"alice",
		"bob",
		"chen",
		"drake",
		"eve",
		"fyodor",
		"gauri",
		"hari",
		"ishla",
		"jake",
	}
	warray = [...]string{
		"andaman",
		"bhopal",
		"calcutta",
		"delhi",
		"esteban",
	}
	aslice = []string{
		"austria",
		"bangladesh",
		"china",
		"denmark",
		"estonia",
	}
)

func mid_slice(yarray []string) {
	fmt.Println("aslice 2:4 =>", aslice[2:4])
}
func left_slice(yarray [5]string) {
	fmt.Println("warray 2: =>", yarray[2:])
}
func right_slice(yarray [5]string) {
	fmt.Println("warray :3 =>", yarray[:3])
}
func reslice(yarray [10]string) {
	tmp_slice := yarray[3:5:7]
	fmt.Println("len:", len(tmp_slice), "cap:", cap(tmp_slice), " | yarray 3:5:7 =>", tmp_slice)
	tmp_slice = append(tmp_slice, "moo")

	var _tmp_slice = make([]string, 4, 4)
	copy(_tmp_slice, tmp_slice)
	_tmp_slice[3] = "zoo"
	fmt.Println("len:", len(_tmp_slice), "cap:", cap(_tmp_slice), " | : =>", _tmp_slice)

	tmp_slice = _tmp_slice
	fmt.Println("len:", len(tmp_slice), "cap:", cap(tmp_slice), " | : =>", tmp_slice)
}

func main() {
	fmt.Println("yaraay ~")
	for idx, item := range yarray {
		fmt.Printf("[%d] %s ", idx, item)
	}
	fmt.Println("\nwaraay ~")
	for idx, item := range warray {
		fmt.Printf("[%d] %s ", idx, item)
	}
	fmt.Println("\naslice ~")
	for idx, item := range aslice {
		fmt.Printf("[%d] %s ", idx, item)
	}
	fmt.Println("\n~")
	mid_slice(aslice)
	left_slice(warray)
	right_slice(warray)
	reslice(yarray)
}

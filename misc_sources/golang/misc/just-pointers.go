package main

import (
	"fmt"
)

func chainblock2(abc *string) {
	*abc = fmt.Sprintf("%s permanently", *abc)
}

func chainblock1(abc *string) {
	fmt.Println(*abc)
	*abc = fmt.Sprintf("%s used", *abc)
	fmt.Println(*abc)
	chainblock2(abc)
	fmt.Println(*abc)
}

func main() {
	var (
		num     int
		ptr_num *int
		ptr     *int
	)
	num = 100
	ptr_num = &num
	fmt.Println(num, ptr_num, *ptr_num)
	*ptr_num = 10
	fmt.Println(num, ptr_num, *ptr_num)
	fmt.Println(ptr)

	abc := "abc"
	chainblock1(&abc)
	fmt.Println(abc)
}

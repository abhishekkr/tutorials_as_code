package main

import "fmt"

type stat map[string][]int

func dailyStats() stat {
	return stat{
		"jack": []int{0, 1, 1, 2, 3, 5},
		"jill": []int{8, 13, 21, 34, 45},
	}
}

func histogram() {
	hist := make(map[string]int)
	hist["Jan"] = 100
	hist["Feb"] = 445
	hist["Mar"] = 514
	fmt.Println(hist)
}

func existsOrNot() {
	uuid := map[string]int{
		"Able":  2,
		"Blohr": 1,
		"Cain":  0,
	}
	temp_var01 := uuid["Cain"]
	fmt.Println("Cain gets", temp_var01)
	temp_var02 := uuid["Dahl"]
	fmt.Println("Dahl gets", temp_var02)
	_, ok_cain := uuid["Cain"]
	if !ok_cain {
		fmt.Println("Cain doesn't exist.")
	}
	_, ok_dahl := uuid["Dahl"]
	if !ok_dahl {
		fmt.Println("Dahl doesn't exist.")
	}
}

func remove(store map[string][]int, key string) (map[string][]int, error) {
	_, ok := store[key]
	if !ok {
		return nil, fmt.Errorf("Key not found")
	}
	delete(store, key)
	return store, nil
}

func main() {
	fmt.Println(dailyStats()["jill"])
	histogram()
	existsOrNot()
	jill, err := remove(dailyStats(), "jack")
	fmt.Println(jill, err)
}

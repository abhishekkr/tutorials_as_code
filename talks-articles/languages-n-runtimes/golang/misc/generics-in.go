package main

import "fmt"

type LinkedList struct {
	value interface{}
	next  *LinkedList
}

func (oldNode *LinkedList) prepend(value interface{}) *LinkedList {
	return &LinkedList{value, oldNode}

}

func tail(value interface{}) *LinkedList {
	return &LinkedList{value, nil}

}

func traverse(ll *LinkedList) {
	if ll == nil {
		return

	}
	fmt.Println(ll.value)
	traverse(ll.next)

}

func main() {
	node := tail(5).prepend(6).prepend(7)
	traverse(node)

}

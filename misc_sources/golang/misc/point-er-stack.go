package main

import (
	"fmt"
	"reflect"
)

////////////////////////////////////////////////////////////////////////////////

type Dir struct {
	*dir
}

type dir struct {
	dirName string
	info    *dirInfo // nil unless directory being read
}

type dirInfo string

func whatDir(dir01 *dir, name string, path dirInfo) {
	fmt.Println("oye lucky lucky oye")
	dir01.dirName = name
	(*dir01).info = &path
	//*((*dir01).info) = path
	//*(*dir01).info = path
	fmt.Println(reflect.TypeOf(*((*dir01).info)))
}

func example01() {
	d01 := dir{}
	whatDir(&d01, "hey", "/tmp/hey")
	fmt.Println(d01.dirName, *d01.info)
}

////////////////////////////////////////////////////////////////////////////////
// source: http://openmymind.net/Things-I-Wish-Someone-Had-Told-Me-About-Go/

type User struct {
	Name string
}

func Modify(u *User) {
	u = &User{Name: "Paul"}
}

func ModifyForever(u **User, towhat string) {
	*u = &User{Name: towhat}
}

func example02() {
	u := &User{Name: "Leto"}
	println(u.Name)
	Modify(u) //passes pointer by Value not Reference so change doesn't come back
	println(u.Name)
	ModifyForever(&u, "Paul")
	println(u.Name)
}

////////////////////////////////////////////////////////////////////////////////
// http://golangtutorials.blogspot.in/2011/06/methods-on-structs.html
type Rectangle struct {
	length, width int
}

func (r Rectangle) Area_by_value() int {
	return r.length * r.width
}

func (r *Rectangle) Area_by_reference() int {
	return r.length * r.width
}

func example03() {
	r1 := Rectangle{4, 3}
	fmt.Println("Rectangle is: ", r1)
	fmt.Println("Rectangle area is: ", r1.Area_by_value())
	fmt.Println("Rectangle area is: ", r1.Area_by_reference())
	fmt.Println("Rectangle area is: ", (&r1).Area_by_value())
	fmt.Println("Rectangle area is: ", (&r1).Area_by_reference())
}

////////////////////////////////////////////////////////////////////////////////

func main() {
	example01()
	example02()
	example03()
}

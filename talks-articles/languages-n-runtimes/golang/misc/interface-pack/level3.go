package main

import "fmt"

/*
embedded inner type's implementation of interface is "promoted" to outer type
*/

var (
	Accounts = map[string]string{
		"James": "Bond",
	}
)

type user interface {
	Login(string)
	logout()
}

/* REGISTERED USER *******/
type registeredUser struct {
	id         string
	isLoggedIn bool
}

func (usr *registeredUser) Login(password string) {
	if Accounts[usr.id] == password {
		usr.isLoggedIn = true
		fmt.Println("Login Successful:", usr.id)
	}
}

func (usr *registeredUser) logout() {
	usr.isLoggedIn = false
	fmt.Println("Logout Successful:", usr.id)
}

/* /REGISTERED USER *******/

/* ADMIN USER *******/
type adminUser struct {
	registeredUser
	privilege map[string]string
}

/* /ADMIN USER *******/

/* GUEST USER *******/
type guestUser struct {
	registeredUser
}

func (usr *guestUser) Login(_ string) {
	usr.id = "Guest"
	usr.isLoggedIn = true
	fmt.Println("Login Successful:", usr.id)
}

/* /GUEST USER *******/

func loginLogout(usr user, password string) {
	usr.Login(password)
	usr.logout()
}

func main() {
	admin := adminUser{
		registeredUser: registeredUser{
			id:         "James",
			isLoggedIn: false,
		},
		privilege: map[string]string{
			"allow": "all",
		},
	}
	loginLogout(&admin, Accounts[admin.registeredUser.id])
	// also can be run as obviously
	// admin.registeredUser.Login(Accounts[admin.registeredUser.id])
	// admin.Login(Accounts[admin.registeredUser.id])
	guest := guestUser{
		registeredUser: registeredUser{
			id:         "James",
			isLoggedIn: false,
		},
	}
	loginLogout(&guest, "")
}

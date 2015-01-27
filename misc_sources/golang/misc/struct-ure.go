package main

import "fmt"

type Contact struct {
  Name string
  Phone []string
  EmailId string
}

type Identity struct {
  Contact
  Nationality string
  PassportNumber string
}

func (contact Contact) NameIs(){
  fmt.Println(contact.Name)
}

func main(){
  var bob Contact
  bob.EmailId = "bob@b0b.org"

  alice := Contact{}

  eve := Contact{
    Name: "Eve",
    Phone: []string{"+01-01010101", "+02-020202020"},
    EmailId: "eve@eva.com",
  }

  trudy := &eve //pointer
  chuck := new(Contact) //pointer

  fmt.Println(bob, alice, eve, trudy, chuck)
  eve.NameIs()

  obama := new(Identity)
  obama.Name = "Obama"
  obama.Nationality = "US"

  fmt.Println(obama)
  obama.NameIs()
}

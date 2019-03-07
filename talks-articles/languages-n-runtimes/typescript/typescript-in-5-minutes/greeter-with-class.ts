class Person {
  title: string;
  fullName: string;
  constructor(public titl: string,
              public firstName: string,
              public midName: string,
              public lastName: string) {
    this.title = titl;
    this.fullName = firstName + " " + midName + " " + lastName;
  }
}
interface PeopleName {
  firstName:  string;
  lastName:   string;
  title:      string;
}

function greeter(myname: PeopleName) {
  return "Heya " + myname.title +  " " + myname.firstName + " " + myname.lastName;
}

let someone = new Person("Mr.", "James", "L.", "Bond");

document.body.innerHTML = greeter(someone);

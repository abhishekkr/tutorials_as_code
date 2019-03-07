interface PeopleName {
  firstName:  string;
  lastName:   string;
  title:      string;
}

function greeter(myname: PeopleName) {
  return "Heya " + myname.title +  " " + myname.firstName + " " + myname.lastName;
}

let someName = { firstName: "James", lastName: "Bond", title: "Mr." };

document.body.innerHTML = greeter(someName);

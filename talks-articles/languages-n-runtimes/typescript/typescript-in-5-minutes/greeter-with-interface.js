function greeter(myname) {
    return "Heya " + myname.title + " " + myname.firstName + " " + myname.lastName;
}
var someName = { firstName: "James", lastName: "Bond", title: "Mr." };
document.body.innerHTML = greeter(someName);

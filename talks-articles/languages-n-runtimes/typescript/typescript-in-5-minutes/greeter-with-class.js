var Person = /** @class */ (function () {
    function Person(titl, firstName, midName, lastName) {
        this.titl = titl;
        this.firstName = firstName;
        this.midName = midName;
        this.lastName = lastName;
        this.title = titl;
        this.fullName = firstName + " " + midName + " " + lastName;
    }
    return Person;
}());
function greeter(myname) {
    return "Heya " + myname.title + " " + myname.firstName + " " + myname.lastName;
}
var someone = new Person("Mr.", "James", "L.", "Bond");
document.body.innerHTML = greeter(someone);

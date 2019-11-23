import ballerina/io;

function fnSome(int uid, string name="John", string... friends) {
  io:println("uid: ", uid, " | name: ", name, " has ", friends.length(), " friends");
  io:println(friends);
}

public function main() {
  fnSome(1);
  fnSome(2, name="Jack");
  fnSome(3, "Alice");
  fnSome(4, "Alice", "Bob");
  // fnSome(5, name="Jack", "Alice", "Bob"); // not allowed as positional argument not allowed after named arguments

  string[] friends = ["Alice", "Bob", "Eve"];
  fnSome(6, "Dory", ...friends);
}

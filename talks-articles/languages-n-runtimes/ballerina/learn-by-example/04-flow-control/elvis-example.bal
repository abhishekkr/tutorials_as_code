import ballerina/io;

public function main() {
  string|() s1 = ();

  string s2 = s1 is string ? s1 : "default"; // type-guard for value of s1 or default
  io:println(s2);

  string elvisResult1 = s1 ?: "elvis-default"; // same via elvis
  io:println(elvisResult1);
}

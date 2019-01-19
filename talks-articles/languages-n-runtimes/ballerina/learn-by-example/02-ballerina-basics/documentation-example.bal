import ballerina/io;

# `printSum` is a local function which prints sum of all integers passed to it
#
# + nums - This is a rest parameter of type integer
function printSum(int... nums) {
  var sum = 0;
  foreach var num in nums {
        sum += num;
  }
  io:println("sum: ", sum);
}

public function main() {
  printSum();
  printSum(1);
  printSum(1, 2);
  printSum(1, 4, 7, 9);
}

import ballerina/io;

int globalTimes = 2;

function basicClosure() returns (function () returns int) {
  int someVar = 10;
  var globalTimesSomeVar = function () returns int {
    return globalTimes * someVar;
  };
  return globalTimesSomeVar;
}

function multiLevelClosure(int x) returns (function () returns int) {
  var fnX = function () returns int {
    var fnY = function () returns int {
      var fnZ = basicClosure();
      return fnZ.call();
    };
    return fnY.call() * x;
  };
  return fnX;
}

function fnPointer() returns (function () returns (function () returns (int))) {
  var fnX = function () returns (function () returns (int)) {
    return multiLevelClosure(globalTimes);
  };
  return fnX;
}

public function main() {
  var resultBasicClosure = basicClosure();
  io:println("basic: ", resultBasicClosure.call());

  var resultMultiLevelClosure = multiLevelClosure(100);
  io:println("basic: ", resultMultiLevelClosure.call());

  var resultFnPointerClosure = fnPointer();
  var resultFnOfFnPointerClosure = resultFnPointerClosure.call();
  io:println("basic: ", resultFnOfFnPointerClosure.call());
}

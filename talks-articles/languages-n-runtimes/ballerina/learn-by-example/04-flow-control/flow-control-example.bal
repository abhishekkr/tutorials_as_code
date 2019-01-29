import ballerina/io;

function ifelseExample() {
  var xyz = 1;

  if (xyz == 1) {
    io:println("simple check");
  }

  xyz = 500;
  if (xyz == 100) {
    io:println("eq 100");
  } else if (xyz > 1000) {
    io:println("big enough");
  } else {
    io:println("other case");
  }

  io:println(xyz);
}

function whileExample() {
  var xyz = 10;

  while (xyz < 5) {
    xyz = xyz + 1;
    if (xyz == 3) {
      continue;
    }
    if (xyz == 4) {
      break;
    }
    io:println(xyz);
  }
}

public function main() {
  ifelseExample();
  whileExample();
}

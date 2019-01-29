import ballerina/io;

type RecordA record {
  string var1;
};

type RecordB record {
  int var1;
  !...
};

type RecordC record {
  string var1;
  RecordB var2;
};

function basicRecordMatch(any r) {
  match r {
    var {var1, var2, var3} => io:println("rec with 3: " + io:sprintf("%s",var1) + ", " + io:sprintf("%s",var2) + ", " + io:sprintf("%s",var3));
    var {var1, var2} => io:println("rec with 2: " + io:sprintf("%s",var1) + ", " + io:sprintf("%s",var2));
    var {var1} => io:println("rec with 1: " + io:sprintf("%s",var1));
    _ => io:println("unmatched record", r);
  }
}

function basicRecordMatchWithType(any r) {
  match r {
    var {var1, var2} if (var1 is string && var2 is RecordB) => io:println("rec with 2: " + io:sprintf("%s",var1) + ", " + io:sprintf("%s",var2));
    var {var1} if var1 is string => io:println("rec with 1: " + io:sprintf("%s",var1));
    _ => io:println("unmatched record", r);
  }
}

function recordMatchExample() {
  RecordA a1 = {var1: "something"};
  RecordA a2 = {var1: "something", var2: 1};
  RecordA a3 = {var1: "something", var2: true, var3: 1.1};
  basicRecordMatch(a1);
  basicRecordMatch(a2);
  basicRecordMatch(a3);

  RecordB b1 = {var1: 10};
  RecordC c1 = {var1: "abc", var2: b1};
  basicRecordMatchWithType(a1);
  basicRecordMatchWithType(b1);
  basicRecordMatchWithType(c1);
}

public function main() {
  recordMatchExample();
}

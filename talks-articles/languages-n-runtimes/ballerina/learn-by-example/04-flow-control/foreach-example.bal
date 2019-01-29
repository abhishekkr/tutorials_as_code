import ballerina/io;

// foreach
function foreachExample(){
  json cX = {a: "alice", friends: ["bob", "eve"], uid: 190};

  io:println("iterate over json:");
  map <json> mapJson = <map<json>> map<json>.convert(cX);
  foreach var (k, v) in mapJson {
    if (v is string) {
      io:println("string type: ", v);
    } else if (v is int) {
      io:println("int type: ", v);
    } else if (v is json[]) {
      io:println("json[] type: ", v);
    } else {
      io:println("unsupported type: ", v);
    }
  }

  io:println("iterate over json array:");
  json[] friends = <json[]>cX.friends;
  foreach var f in friends {
    io:println("+ ", f);
  }

  io:println("iterate over xml");
  xml months = xml `<months><month>
      <name>january</name>
      <days>31</days>
    </month></months>`;
  foreach var m in months.*.*.elements() {
    io:println(".. ", m);
  }

  io:println("iterate over closed int range");
  var fin = 7;
  foreach var i in 1...fin {
    fin = fin + i;
  }
  io:println("(above once fin inferred for range, doesn't change) | fin: ", fin);
  foreach var i in 1...3 {
    io:println(":: ", i);
  }

  io:println("iterate over half open range, not including end");
  foreach var i in 1..<3 {
    io:println(": ", i);
  }
}

public function main() {
  foreachExample();
}

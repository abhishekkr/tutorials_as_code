import ballerina/io;

function toUpper((string, string) pair) returns string {
    var (key, value) = pair;
    return value.toUpper();
}

function useMap() {
  map<string> speakers = {
    erlang: "joe armstrong",
    haskell: "phlip wadler",
    go: "rob pike",
    clojure: "rich hickey"
  };

  io:println("available speakers are: ", speakers.count());

  string[] speakersWithCase = speakers.map(toUpper);
  io:println(speakersWithCase);
}

function useArray() {
  int[] xarr = [-100, -11, 0, 5, 23];
  float avg = xarr.filter(
      function (int i) returns boolean { return i >= 0; }
      ).average();
  var xarrSum = xarr.sum();
  var xarrMax = xarr.max();
  var xarrMin = xarr.min();

  io:println("avg: ", avg, " | sum: ", xarrSum);
  io:println("max: ", xarrMax, " | min: ", xarrMin);
}

function useJson() {
  map<json> j = {name: "James", skills: ["offence", "defense"], id: 7};

  j.map(function ((string, json) pair) returns string {
      var (k, v) = pair;
      return v.toString();
    }).foreach(function (string s) {
      io:println("[+] ", s);
    });
}

public function main() {
  useMap();
  useArray();
  useJson();
}

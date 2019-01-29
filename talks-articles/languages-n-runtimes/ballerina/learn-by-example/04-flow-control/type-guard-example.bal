import ballerina/io;

// structures/data for type-guard
type Time record {
  int year;
  Date? date;
  Event? event;
};

type Date record {
  string month;
  int day;
};

type Event record {
  string msg;
};

type ErrorKeyNotFound error<string, record{ string msg; }>;

// function using type-guard structures/data
function typeForTime(Time t) returns (Date?|Event|error) {
  Event? ev = t.event;
  if (t.date is Date) { // type-guard checking Date type
    return t.date;
  } else if (ev is Event) {
    return ev;
  }

  error noTimeFound = error("no time found");
  return noTimeFound;
}

function getValue(map<string?> keyvals, string key) returns string?|ErrorKeyNotFound {
  if (!keyvals.hasKey(key)) {
    ErrorKeyNotFound err = error("key '" + key + "' not found", { msg: key });
    return err;
  } else {
      return keyvals[key];
  }
}

function stringPrint(string?|ErrorKeyNotFound result) {
  if (result is string) {
    io:println(result);
  } else if (result is string) {
    io:println("value is ()");
  } else if (result is ErrorKeyNotFound) {
    io:println(result.reason());
  }
}

public function main() {
  Time time1 = {year: 2009,
    date: {month: "mar", day: 24},
    event: {msg: "say-what"}};
  var t1 = typeForTime(time1);
  io:println(t1);

  Time time2 = {year: 2009,
    date: (),
    event: {msg: "say-what"}};
  var t2 = typeForTime(time2);
  io:println(t2);

  Time time3 = {year: 2009, date: (), event: ()};
  var t3 = typeForTime(time3);
  io:println(t3);

  map<string?> values = {"main": "wow", "extra": ()};
  var s = getValue(values, "main");
  var z = getValue(values, "mains");

  stringPrint(s);
  stringPrint(());
  stringPrint(z);
}

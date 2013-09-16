package joinr

import "testing"

type testPair struct {
  strs []string
  expected string
}

var tests = []testPair{
  {[]string{"abcde", "fghij"}, "abcdefghij"},
  {[]string{"ab", "cd", "ef", "gh", "ij"}, "abcdefghij"},
}

func TestJoin(t *testing.T) {
  pair := tests[0]
  result := Join(pair.strs[0] + pair.strs[1])
  if pair.expected != result {
    t.Error("Expected", pair.expected, "but got", result)
  }
}

func TestJoinArr(t *testing.T) {
  pair := tests[1]
  result := Join(pair.strs[0] + pair.strs[1] + pair.strs[2] + pair.strs[3] + pair.strs[4])
  if pair.expected != result {
    t.Error("Expected", pair.expected, "but got", result)
  }
}

package sample

import (
	"testing"
	"testing/quick"
)

func TestEmptyLen(t *testing.T) {
	eLen := func(key []string) bool {
		// not correct to test package but ok for quick
		return EmptyLen(key) <= len(key)
	}

	if err := quick.Check(eLen, nil); err != nil {
		t.Error(err)
	}
}

func BenchmarkEmptyLen(b *testing.B) {
	for i := 0; i < b.N; i++ {
		lst := make([]string, i)
		EmptyLen(lst)
	}
}

func TestNotEmptyLen(t *testing.T) {
	eLen := func(key []string) bool {
		// not correct to test package but ok for quick
		return NotEmptyLen(key) <= len(key)
	}

	if err := quick.Check(eLen, nil); err != nil {
		t.Error(err)
	}
}

func BenchmarkNotEmptyLen(b *testing.B) {
	for i := 0; i < b.N; i++ {
		lst := make([]string, i)
		NotEmptyLen(lst)
	}
}

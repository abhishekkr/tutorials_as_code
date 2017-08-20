package ex

import (
	"sort"
	"testing"
	"testing/quick"

	"github.com/ryszard/goskiplist/skiplist"
)

func TestOrdering(t *testing.T) {
	scenario := func(in []int) bool {
		var (
			found     int
			no        = len(in)
			reference = make([]int, no)
			skiplist  = skiplist.NewIntSet()
		)
		copy(reference, in)
		sort.Ints(reference)

		for _, v := range in {
			skiplist.Add(v)
		}
		for it := skiplist.Iterator(); it.Next(); {
			got := it.Key().(int)
			// first invariant
			if found > no {
				t.Fatal("skiplist contains more items than were given as input")
				return false
			}
			// second invariant
			if want := reference[found]; got != want {
				t.Fatalf("skiplist at %d got %d, want %d", found, got, want)
				return false
			}
			found++
		}
		// third invariant
		if found < no {
			t.Fatalf("skiplist had insufficient elements: got %d, want %d", found, no)
			return false

		}
		return true
	}
	if err := quick.Check(scenario /* configuration */, nil); err != nil {
		t.Fatal(err)
	}
}

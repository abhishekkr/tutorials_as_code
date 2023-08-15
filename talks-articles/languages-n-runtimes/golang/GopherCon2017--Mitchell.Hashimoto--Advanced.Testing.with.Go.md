
## Advanced Testing with Go
> by, Mitchell Hashimoto
> Gophercon 2017 Denver Co

### SubTests

new in Go 1.8

let you nest subtests within a test

```
func TestAdd (t *testing.T) {
  a := 1

  t.Run("+1", func (t *testing.T){
    if a + 1 != 2 { t.Fatal("fail") }
  })

  t.Run("+2", func (t *testing.T){
    if a + 2 != 3 { t.Fatal("fail") }
  })
}

// can still do target run
// go test -run=TestAdd/+1
```

can use defers among subtests

---

### Table Driven Tests

good use-case for subtests

```
func TestAdd (t *testing.T) {
  cases := []struct{ A, B, Expected int }{
    {1, 1, 2},
    {1, -1, 0},
    {1, 0, 1},
    {0, 0, 0},
  }

  for _, tc := range cases {
    t.Run(fmt.Sprintf("%d + %d", tc.A, tc.B), func(t *testing.T) {
      actual := tc.A + tc.B
      if actual != tc.Expected { t.Fatal("fail") }
    })
  }
}

// can still do target run
// go test -run=TestAdd/+1
```

* low overhead to add new test cases
* makes testing exhaustive scenarios simple

#### Consider naming the cases

auto-generated names aren't that specific and easier to find

```
func TestAdd (t *testing.T) {
  cases := []struct{
    Name string
    A, B, Expected int
  }{
    {"case1", 1, 1, 2},
    {"case-foo", 1, -1, 0},
    {"case-off", 1, 0, 1},
    {"case-nil", 0, 0, 0},
  }

  for _, tc := range cases {
    t.Run(tc.Name, func(t *testing.T) {
      actual := tc.A + tc.B
      if actual != tc.Expected { t.Fatal("fail") }
    })
  }
}
```

---

### Test-Fixtures

access data from external sources
* data separate from logic
* could be extensively generated and maintained

```
func TestAdd (t *testing.T) {
  data := filepath.Join("test-fixtures", "add_data.json")
  ...
}
```


---
---

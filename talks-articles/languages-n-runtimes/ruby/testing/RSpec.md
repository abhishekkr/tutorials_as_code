## RSpec

> ChefSpec and ServerSpec based on its DSL.

#### DSL

> _Setup_ -> _Execute_ -> _Expectation_

* Sample DSL

```
describe 'x times 10' do
  it 'equals x0' do
    ## setup
    x = 10

    ## execute
    x_time_10 = x * 10

    ## expectation
    expect(x_time_10).to eq(100)
  end

  it 'not equals x1' do
    x = 10
    x_time_10 = x * 10

    expect(x_time_10).to eq(101)
  end
end
```

* Context an alias of describe allows categorize smaller groups, can use 'let' to setup

```
describe 'x times 10' do
  context "where x is a number" do
    let(:x){10}
    let(:x_time_10){ x * 10 } ## creates memoized x_time_10

    it 'equals x0' do
      expect(x_time_10).to eq(100)
    end

    it 'not equals x1' do
      expect(x_time_10).to eq(101)
    end
  end
end
```

---

#### console utility

* by default assumes subdir 'spec'
```
rspec
```

* allows to provide one or more paths, specific tests
```
rspec spec/unit/default.rb spec/unit/this_case

rspec spec/unit/default.rb:16
```

* with Documentation format
```
rspec -f documentation
```

* colorized output
```
rspec --color
```

---
---

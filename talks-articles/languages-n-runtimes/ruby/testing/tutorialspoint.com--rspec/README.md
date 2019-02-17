
## RSpec Tutorial

[source](https://www.tutorialspoint.com/rspec/)

> at some places pulled in better content from [relishapp.com](https://relishapp.com/rspec/rspec-core/docs/)

* a BDD tool to test app behavior


### Introduction

* install `ruby`, install respec `gem install rspec`

* a hello world example run with `rspec` at [01-hello-world.rb](01-hello-world.rb)

```
$ rspec spec 01-hello-world.rb

Finished in 0.00119 seconds (files took 0.13699 seconds to load)
1 example, 0 failures
```

* a hello world example run with `ruby` needs example at [02-hello-world.rb](02-hello-world.rb)

```
$ ruby 01-hello-world.rb

Finished in 0.00119 seconds (files took 0.13699 seconds to load)
1 example, 0 failures
```

---

### Basic Syntax

* `describe` is used to define an example group, a collection of tests

> * it can take a class name and/or string argument
>
> * a block argument passed to it with individual tests as examples


* `context` with similar notation of `describe`, used to enclose tests of a certain type

> * it's not mandatory but better to classify when there are large sets of tests


* `it` defines tests; this also like `describe` and `context` can accept both class name or a string arg

* `expect` allows to define an expectation as `expect(2+3).to eql 5`, here `eql` is a matcher

---

### Writing Specs

* say if you create a new class, [string_analyzer.rb](./03-writing-specs/string_analyzer.rb)

* rspec for the class above, [string_analyzer_spec.rb](./03-writing-specs/string_analyzer_spec.rb)

##### RSpec flags

> * `-l PATH` to load for source ruby files
>
> * `-r PATH` to require a specific file in spec
>
> * `-f FORMATTER` allows specify different output formats
>
> * `-o FILE` directs rspec to write test results
>
> * `-c | --color` enables colors in output
>
> * `-b | --backtrace` to display full error
>
> * `-w | --warnings` to display rspec warning
>
> * `-P pattern` to load and run files that match pattern
>
> * `-e STRING` directs rspec to run all examples containing string
>
> * `-t TAG` to run specs containing the tag specified as ruby symbol

---

### Matchers

* used as `expect($value).to $MATCHER $expected`, example at [04-matchers.rb](./04-matchers.rb)


##### Equality/Identity matchers

* `eq` passes `==` operator

* `eql` passes `<val>.eql? result`

* `be` and `equal` passes `<val>.equal? result`


##### Comparison matchers

* `be >`, `be >=`, `be <`, `be <=` compare respective truths

* `be_between(RANGE_START, RANGE_END).inclusive` and `be_between(RANGE_START, RANGE_END).exclusive` passes for in the ranges with inclusive and exclusive limits respectively

* `match(/regex/)` passes for reg-exp match


##### Class/Type matchers

* `be_instance_of` passes when an instance

* `be_kind_of` passes hn an instance or any parent classes

* `respond_to` passes when responds to specified method


#### True/False/Nil matchers

* `be true` passes when true; `be false` when false

* `be_truthy` when not false or nil; `be_falsey` when false or nil

* `be_nil` when nil


##### Error matchers

* `raise_error(ErrorClass)` passes if an error of ErrorClass is raised

* `raise_error("ErrorMessage")` passes when an error raised with message

* `raise_error(ErrorClass, "ErrorMessage")` passes when ErrorClass error raised with ErrorMessage

---

### Test Doubles/Mocks

* `double` to create a mock call handler

* strict by default not allowing anything undefined, can be configured `loose`

* if mocked method doesn't gets used, tests fail

* example [05-edibles.rb](./05-edibles.rb) to showcase

---

### Stubs

* `stub` is a special type that stand-in for existing method

* if stubs method doesn't gets used, tests do not fail

* example [06-stubs.rb](./06-stubs.rb) to showcase

---

### Hooks

* most common hooks used in RSpec are before and after hooks to setup and teardown state for tests

* `after(:each)` and `before(:each)` runs before/after each example

* `after(:all)` and `before(:all)` runs before/after all examples

* example [07-hooks.rb](./07-hooks.rb) to showcase

---

### Tags

* let's tag tests and run only a subset

* example [08-tags.rb](./08-tags.rb) to showcase

* run as `rspec --tag smoke spec 08-tags.rb ## for smoke tagged tests`

---

### Subjects

* `subjects` are shortcut to write simple tests, moving object instantiation into `describe` line

* example [09-subjects.rb](./09-subjects.rb) to showcase

---

### Helpers

* to allow re-usable code to be shared across examples

* example [10-helpers.rb](./10-helpers.rb) to showcase

---

### Metadata

* refers to data about data, i.e. info about `describe`, `context`, `it` blocks

* example [11-metadata.rb](./11-metadata.rb) to showcase

---

### Filtering

* done based on RSpec metadata

* example [examples_with_focus.rb](./12-filtering/examples_with_focus.rb) to showcase running focused set of tests of any RSpec group

> * before/after hooks of unmatched group ain't run either
>
> * can also just use `:symbols` instead of `key: val`

* example [examples_with_exclude.rb](./12-filtering/examples_with_exclude.rb) to showcase running not excluded set of tests of any RSpec group

* `filter_run_when_matching` config allows to focus if any metadata matching found, else runs all

* can filter RSpec config to be specific for example groups as in [examples_hook_filter.rb](./12-filtering/examples_hook_filter.rb)

---

### Expectations

* is simply a statement in `it` block using `expect()`, it's defining an expectation of a behavior

---

### Formatting

* `prgress` is default

* `doc` or `documentation` provides more structured out which can be saved with `--out file` if needed

* can provide multiple formats at once

```
rspec --format progress --format documentation spec file_spec.rb
```

---

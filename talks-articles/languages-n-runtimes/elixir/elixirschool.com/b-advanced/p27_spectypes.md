
## Specifications and Types

> [sample code](./p27_spectypes.exs)

* `@spec` is syntax complement for writing documentation, could be analyzed at compilation time for function spec

* `@type` helps writing better code


### Specification

* to define spec `@spec function_name(param_type) :: return_type` directive need to be placed before function definition

* example `ExampleSpec.plus_10/1` has sample spec definition

* tools like `Dialyzer` perform static analysis of code to find related bugs


### Custom Types

* `@type` notation as under `TypeX` module in sample code gets used for available Type spec which could be used later as in `ExampleTypeX.greet/2`

#### Defining custom types

* `@type` defines simple public type

* `@typep` defines private types

* `@opaque` defines public type with private internal structure

#### Documentation of types

* `@typedoc` is available to specify documentation for custom type

---


## Protocols

> [sample code](p30_protocols.exs)

* are a way to achieve polymorphism, to extend an existing API for new types

* for no implentation on a type, `protocol not implemented` error gets raised


### Implementing a Protocol

* pre-existing protocol for a new type can be implemented using `defimpl` as for `String.Chars` in sample code allowing `to_string()` for custom type

* `defprotocol` to implement a new protocol

> Structs are not enumerable and can't be accessed using an implementation as `to_atom` in sample code for Map, although underneath structs are Maps

---

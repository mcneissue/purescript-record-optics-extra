# extra-record-optics

## How to install
TODO: Uhhhhh

## How to use

```purescript
> import Data.Lens.Record.Extract
> import Data.Lens.Record.Remap
> import Data.Symbol
> import Data.Lens

> view (remap { foo: SProxy :: _ "bar" }) { foo: 42, baz: "something" }
{ bar: 42, baz: "something" }

> # the value on the rhs of the record is meaningless, you can put anything there
> view (extract { foo: SProxy }) { foo: 42, baz: "something" }
{ foo: 42 }

> scheme = { foo: SProxy :: _ "bar" }
> set (remap scheme >>> extract scheme) { bar: "potato" } { foo: 42, baz: "quux" }
{ baz: "quux", foo: "potato" }
```

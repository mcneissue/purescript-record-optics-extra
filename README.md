# record-optics-extra

## How to install
TODO: Uhhhhh

## How to use

```purescript
> import Data.Lens
> import Data.Symbol
> import Data.Lens.Record.Extra

> view (remappedBy { foo: SProxy :: _ "bar" }) { foo: 42, baz: "something" }
{ bar: 42, baz: "something" }

> # the value on the rhs of the record is meaningless, you can put anything there
> view (extractedBy { foo: SProxy }) { foo: 42, baz: "something" }
{ foo: 42 }

> scheme = { foo: SProxy :: _ "bar" }
> set (remappedBy scheme >>> extractedBy scheme) { bar: "potato" } { foo: 42, baz: "quux" }
{ baz: "quux", foo: "potato" }
```

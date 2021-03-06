# Changelog for inline-asm

## v0.4.0.2

* Fix compatibility with the recently released template-haskell-2.16.0.0.

## v0.4.0.1

* Fix expression parser not parsing some expressions it definitely should parse.

## v0.4.0.0

* Add helpers (`unroll`/`unrolls`) for compile-time loop unrolling.
* Add `move` remapping command.
* Use `{}` for escaping instead of `${}` to improve readability.

## v0.3.1.0

* Support passing `ByteString`s to the assembly.
* Introduced a helper synonym `RET_HASK` to get back into the Haskell land explicitly.

## v0.3.0.0

* Support named arguments for doubles and floats as well.
* Support passing Ptr values.

## v0.2.0.0

* Support named arguments.

## v0.1.1.0

* Support returning tuples.

## v0.1.0.0

* Initial implementation.

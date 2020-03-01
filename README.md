# inline-asm

[![Build Status][travis-badge]][travis]

_When inline C is too safe_.

Did you try `inline-c`, but it's not enough? You need more? Nothing seems to satisfy?
`inline-asm` to the rescue!

And, since the inline assembly is just a usual Haskell value (even if manipulated at compile-time),
there's a lot of pretty cool stuff one can do, like, for instance, explicit compile-time loop unrolling.

## Examples

Swapping two `Int`s and also incrementing one of them by two:
```haskell
defineAsmFun "swap2p1"
  [asmTy| (a : Int) (b : Int) | (_ : Int) (_ : Int)]
  [asm|
  xchg {a}, {b}
  add $2, {b}
  |]
```
(note the `{a}`, `{b}` antiquoters)

Getting the last character of a `ByteString`, or a default character if it's empty:
```haskell
defineAsmFun "lastChar"
  [asmTy| (bs : ByteString) (def : Word) | (w : Word) |]
  [asm|
  test {bs:len}, {bs:len}
  jz is_zero
  movzbq -1({bs:ptr},{bs:len}), {w}
  RET_HASK
is_zero:
  mov {def}, {w}
  |]
```
(note the special `{bs:ptr}` and `{bs:len}` antiquoters, as well as `RET_HASK` command to return early)

SIMD-accelerated character occurrences count in a string:
```haskell
defineAsmFun "countCharSSE42"
  [asmTy| (ch : Word8) (ptr : Ptr Word8) (len : Int) | (cnt : Int) |] $
  unroll "i" [12..15]
  [asm|
  push %r{i}|] <> [asm|
  vmovd {ch}, %xmm15
  vpxor %xmm0, %xmm0, %xmm0
  vpshufb %xmm0, %xmm15, %xmm15

  shr $7, {len}

  mov $16, %eax
  mov $16, %edx

  xor {cnt}, {cnt}

  {move ptr rdi}
loop: |] <> unrolls "i" [1..8] [
  [asm|
  vmovdqa {(i - 1) * 0x10}({ptr}), %xmm{i}
  |], [asm|
  vpcmpestrm $10, %xmm15, %xmm{i}
  vmovdqa %xmm0, %xmm{i}
  |], [asm|
  vmovq %xmm{i}, %r{i + 7}
  |], [asm|
  popcnt %r{i + 7}, %r{i + 7}
  |], [asm|
  add %r{i + 7}, {cnt}
  |]
  ] <>
  [asm|
  add $128, {ptr}
  dec {len}
  jnz loop|] <> unroll "i" [15,14..12] [asm|
  pop %r{i} |]
```
(note the `unroll`/`unrolls` Haskell function for compile-time code generation and loop unrolling
with arithmetic expressions in the templates)

## Basic usage

The entry point is the `defineAsmFun` function from `Language.Asm.Inline`
as well as the `asm` and `asmTy` quasiquoters from `Language.Asm.Inline.QQ`.

First, enable some extensions required for Template Haskell and for the unlifted FFI marshalling stuff:
```haskell
{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
{-# LANGUAGE GHCForeignImportPrim, UnliftedFFITypes, UnboxedTuples #-}
```

then one can just do
```haskell
defineAsmFun "funName"
  [asmTy| (someInt : Int) (somePtr : Ptr Word) (someStr : BS.ByteString) | (len : Int) (count : Int) |]
  [asm|
  ; your asm code follows
  |]
```
to define a function `funName` of the type `Int -> Ptr Word -> BS.ByteString -> (Int, Int)`.

### Antiquotation

Good news: it's not necessary to memorize the GHC calling convention
to access the arguments and the output values slots!
Instead, one can use the `{someInt}` syntax to refer to the argument or the return slot named `someInt`.

**NB**: despite that, be careful to not accidentally overwrite an input parameter for now
by picking a wrong register for temporary computations.
We might introduce some syntax to pick unused registers in a future version, but for now care must be taken.

`ByteString` parameters are supported, but, being composite objects, they are a bit special:
an input parameter

Sometimes it might be handy to reassociate an input parameter with another register.
For this, the `{move argName newReg}` antiquoter can be used (for instance, `{move someInt rdi}`).
This will both update the mapping from argument names to register names
as well as issue an assembly `mov` command.

In case you need to return early to Haskell-land, just write `RET_HASK`,
which gets substituted by the actual command to return to Haskell.

### Explicit loop unrolling

The `asm` quasiquoter basically produces a string,
so it can be manipulated at compile-time with arbitrary Haskell functions.
In particular, a string template can be replicated at compile time.

The `unroll` function unrolls a single `asm` code block,
calculating arithmetic expressions involving the unroll variable, so
```haskell
unroll "i" [1..8] [asm|vmovdqa {(i - 1) * 0x10}({ptr}), %xmm{i}|]
```
is equivalent to
```assembly
vmovdqa 0x0({ptr}), %xmm1
vmovdqa 0x10({ptr}), %xmm2
vmovdqa 0x20({ptr}), %xmm3
vmovdqa 0x30({ptr}), %xmm4
vmovdqa 0x40({ptr}), %xmm5
vmovdqa 0x50({ptr}), %xmm6
vmovdqa 0x60({ptr}), %xmm7
vmovdqa 0x70({ptr}), %xmm8
```

`unrolls` works analogously, but it takes a list of `asm` code blocks (instead of a single block),
unrolls each of them and then concatenates the results. Equationally,
```haskell
unrolls var ints codes = foldMap (unroll var ints) codes
```

The `countCharSSE42` function above might be a pretty good example.


## Safety and notes

* First of all, all this is utterly unsafe.
* The compiler sees the generated functions as pure, so if a function calls,
  say, `RDRAND` and is itself called more than once to get several random numbers,
  care must be taken to ensure the compiler doesn't elide extra calls.
  We might introduce some shortcuts to allow wrapping such impure functions
  in an `IO` or `PrimMonad` or soemthing similar.
* Each function is compiled in its own `.S` file,
  so one can freely pick arbitrary naming for the labels and so on,
  but, on the other hand, one cannot access labels in other functions.
  This can be remedied somewhat easily — consider throwing up an issue if that's actually desired.
* Finally, all this is utterly unsafe.

[travis]:        <https://travis-ci.org/0xd34df00d/inline-asm>
[travis-badge]:  <https://travis-ci.org/0xd34df00d/inline-asm.svg?branch=master>

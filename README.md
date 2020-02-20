# inline-asm

_When inline C is too safe_.

Did you try `inline-c`, but it's not enough? You need more? Nothing seems to satisfy?
`inline-asm` to the rescue!

For now the usage is pretty straightforward: use `defineAsmFun` to define the
corresponding function, like
```haskell
defineAsmFun "timesTwo" [t| Word -> Word |] "add %rbx, %rbx"
````
and then use the function `timesTwo` as any other function of type `Word -> Word`:
```haskell
main = print $ timesTwo 21
```

There is also an alternative notation allowing named arguments:
```haskell
defineAsmFun "swap2p1"
  [t| Int -> Int -> (Int, Int) |]
  [asm| a b |
  xchg ${a}, ${b}
  add $1, ${b}
  |]
```

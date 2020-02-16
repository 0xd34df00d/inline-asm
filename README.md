# inline-asm

_When inline C is too safe_.

Did you try `inline-c`, but it's not enough? You need more? Nothing seems to satisfy?
`inline-asm` to the rescue!

For now the usage is pretty straightforward: use `defineAsmFun` to define the
corresponding function, like
```haskell
defineAsmFun "timesTwo" [t| Word -> Word |] "add %rbx, %rbx"
````

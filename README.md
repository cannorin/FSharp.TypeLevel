FSharp.TypeLevel
================

Experimental, work in progress.

See Tests.fs for examples.

## How it works

**TL;DR:** If a type `A` has a static member with signature `Eval: ty<A> -> ty<B>`, then `A` evaluates to the type `B`.

Since F# only computes a type when a (value level) function call happens, we must have a way to treat types as values:

```fsharp
type ty<'t> = struct end

let inline ty<'t> : ty<'t> = ty<'t>()
```

Then our eval function (term level itself, but actually computes the type) looks like:

```fsharp
let inline eval (x: ty< ^A >) : ty< ^B > =
  (^A: (static member Eval: ty< ^A > -> ty< ^B >) x)
```

Let us begin with trivial ones: `True` and `False` are in normal form and evaluate to themselves, so define `Eval` as follows:

```fsharp
type True =
  static member inline Eval (_: ty<True>) = ty<True>

type False =
  static member inline Eval (_: ty<False>) = ty<False>
```

It is convenient to have them act like `car` and `cdr`; recall the Church encoding of boolean.

```fsharp
type True with
  static member inline IfThenElse (_: ty<True>,  x, y) = x 

type False with
  static member inline IfThenElse (_: ty<False>, x, y) = y
```

Then we can implement the `Not<'t>` type:

```fsharp
type Not<'bool> =
  static member inline Eval (_: yy<Not< ^a >>) : yy< ^b >
    when ^a: (static member Eval: ty< ^a > -> ty< ^Bool >) = // (1)
    (^Bool: (static member IfThenElse: ty< ^Bool > * _ * _ -> ty< ^b >)
      ty< ^Bool >, ty<False>, ty<True>) // (2)
```

Two things happen here:

1. Evaluate the inner type `^a` and obtain the normal form `^Bool`
2. Assume `^Bool` has a member `IfThenElse` (which acts like either type-level `car` or `cdr`), then apply `(ty<False>, ty<True>)` to it

and we can compute the negation of a given type-level boolean expression.

```fsharp
let notTrue = eval ty<Not<True>> // ty<False>

let notNotTrue = eval ty<Not<Not<True>>> // ty<True>
```

Similarly we can implement [other boolean operations](https://github.com/cannorin/FSharp.TypeLevel/blob/master/Boolean.fs), [type-level naturals](https://github.com/cannorin/FSharp.TypeLevel/blob/master/Natural.fs), and [type-level lambda calculus](https://github.com/cannorin/FSharp.TypeLevel/blob/master/Lambda.fs), which means the F#'s type system is Turing complete.

## License

Apache 2

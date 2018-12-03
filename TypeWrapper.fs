[<AutoOpen>]
module FSharp.TypeLevel.TypeWrapper

type ty<'t> = struct end with
  override __.ToString() =
     typeof<'t>.ToString()
               .Replace("FSI_0001+", "")
               .Replace("[", "<")
               .Replace("]", ">")
               .Replace("`1", "")
               .Replace("`2", "")

let inline ty<'t> : ty<'t> = ty<'t>()

[<RequireQualifiedAccess>]
module Kind =
  type NotLambdaExpr = interface end
  type Unit = Unit with interface NotLambdaExpr
  type Bool = Bool with interface NotLambdaExpr
  type Nat = Nat with interface NotLambdaExpr
  type List = List with interface NotLambdaExpr
  type LambdaExpr = LambdaExpr

  let inline get (_: ty< ^x >) =
    (^x: (static member Kind: 'kind) ())

  let inline op (k: 'kind, o: 'op, x: ty< ^x >, a: 'arg, d: 'Default) =
    (^x: (static member Op: _*_*_*_*_ -> ty< ^x' >) k,o,x,a,d)

[<RequireQualifiedAccess>]
module Op =
  type Eq = Eq

let inline private eval' (x: ty< ^A >) = (^A: (static member Eval: _ -> ty< ^B >) x)

type Eq<'a, 'b> =
  static member inline Eval (_: ty<Eq< ^l, ^r >>) =
    let l : ty< ^L > = eval' ty< ^l >
    let r : ty< ^R > = eval' ty< ^r >
    let k = Kind.get r
    Kind.op (k, Op.Eq, l, r, ty<Eq< ^L, ^R >>)

module TypeLevelOperators =
  let inline eval' (x: ty< ^A >) = (^A: (static member Eval: _ -> ty< ^B >) x)
  let inline (=^) (x: ty< ^a >) (y: ty< ^b >) =
    eval' ty<Eq< ^a, ^b >>
open TypeLevelOperators

type Eq<'a, 'b> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<Eq< ^h, ^t >>, arg, _) =
    (=^)
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^h >, arg, eval' ty< ^h >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^t >, arg, eval' ty< ^t >))


[<AutoOpen>]
module FSharp.TypeLevel.Natural
open TypeLevelOperators

type Z =
  static member inline Kind = Kind.Nat
  static member inline Eval x = x

type S<'nat> =
  static member inline Kind = Kind.Nat
  static member inline Eval (_: ty<S< ^N >>) : _
    when ^N: (static member Eval: ty< ^N > -> ty< ^N' >) = ty<S< ^N' >>

[<RequireQualifiedAccess>]
module NatOp =
  type Sub = Sub
  type Gt = Gt

let inline private s (x: ty<'x>) = ty<S<'x>>

type Sub<'a, 'b> =
  static member inline Eval (_: ty<Sub< ^x, ^y >>) : _
    when ^x: (static member Eval: ty< ^x > -> ty< ^X >)
     and ^y: (static member Eval: ty< ^y > -> ty< ^Y >) =
    Kind.op (Kind.Nat, NatOp.Sub, ty< ^Y >, ty< ^X >, ty<Sub< ^X, ^Y >>)

type GT<'a, 'b> =
  static member inline Eval (_: ty<GT< ^x, ^y >>) : _
    when ^x: (static member Eval: ty< ^x > -> ty< ^X >)
     and ^y: (static member Eval: ty< ^y > -> ty< ^Y >) =
    Kind.op (Kind.Nat, NatOp.Gt, ty< ^X >, ty< ^Y >, ty<GT< ^X, ^Y >>)

type LT<'a, 'b> =
  static member inline Eval (_: ty<LT< ^x, ^y >>) : _
    when ^x: (static member Eval: ty< ^x > -> ty< ^X >)
     and ^y: (static member Eval: ty< ^y > -> ty< ^Y >) =
    Kind.op (Kind.Nat, NatOp.Gt, ty< ^Y >, ty< ^X >, ty<LT< ^X, ^Y >>)

type GTE<'x, 'y> = Or<GT<'x, 'y>, Eq<'x, 'y>>
type LTE<'x, 'y> = Or<LT<'x, 'y>, Eq<'x, 'y>>

type Z with
  static member inline Op (Kind.LambdaExpr, _, _, _, d) = d
  static member inline Op (Kind.Nat, Op.Eq, _: ty<Z>, _: ty<Z>, _) = true'
  static member inline Op (Kind.Nat, Op.Eq, _: ty<Z>, _: ty<S<_>>, _) = false'
  static member inline Op (KindAbstract.Semigroup, SemigroupOp.Add, _: ty<Z>, x, _) = eval' x
  static member inline Op (Kind.Nat, NatOp.Sub, _: ty<Z>, x, _) = x
  static member inline Op (Kind.Nat, NatOp.Gt, _: ty<Z>, _: ty<Z>, _) = false'
  static member inline Op (Kind.Nat, NatOp.Gt, _: ty<Z>, _: ty<S<_>>, _) = false'
  static member inline Op (Kind.Nat, NatOp.Gt, _: ty<Z>, _: ty< ^tm >, d) : _
    when ^tm: (static member Kind: Kind.LambdaExpr) = d

type S<'nat> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<S< ^n >>, arg, _) =
    s <| Kind.op (Kind.LambdaExpr, op, ty< ^n >, arg, ty< ^n >)
  static member inline Op (Kind.Nat, Op.Eq, _: ty<S<_>>, _: ty<Z>, _) = false'
  static member inline Op (Kind.Nat, Op.Eq, _: ty<S< ^m >>, _: ty<S< ^n >>, _) = eval' ty<Eq< ^m, ^n >>
  static member inline Op (KindAbstract.Semigroup, SemigroupOp.Add, _: ty<S< ^m >>, _: ty< ^n >, _) =
    s <| eval' ty<Add< ^m, ^n >>
  static member inline Op (Kind.Nat, NatOp.Sub, _: ty<S< ^m >>, _: ty<S< ^n >>, _) =
    eval' ty<Sub< ^n, ^m >>
  static member inline Op (Kind.Nat, NatOp.Sub, _: ty<S<_>>, _: ty< ^tm >, d) : _
    when ^tm: (static member Kind: Kind.LambdaExpr) = d
  static member inline Op (Kind.Nat, NatOp.Gt, _: ty<S<_>>, _: ty<Z>, _) = true'
  static member inline Op (Kind.Nat, NatOp.Gt, _: ty<S< ^n >>, _: ty<S< ^m >>, _) =
    eval' ty<GT< ^n, ^m >>
  static member inline Op (Kind.Nat, NatOp.Gt, _: ty<S<_>>, _: ty< ^tm >, d) : _
    when ^tm: (static member Kind: Kind.LambdaExpr) = d

module TypeLevelOperators =
  let Z' = ty<Z>
  let inline S' (_: ty< ^N >) = ty<S< ^N >>
  let inline (-^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<Sub< ^M, ^N >>
  let inline (<^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<LT< ^M, ^N >>
  let inline (>^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<GT< ^M, ^N >>
  let inline (<=^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<LTE< ^M, ^N>>
  let inline (>=^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<GTE< ^M, ^N>>
open TypeLevelOperators

type Sub<'m, 'n> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<Sub< ^h, ^t >>, arg, _) =
    (-^)
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^h >, arg, eval' ty< ^h >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^t >, arg, eval' ty< ^t >))

type GT<'m, 'n> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<GT< ^h, ^t >>, arg, _) =
    (>^)
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^h >, arg, eval' ty< ^h >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^t >, arg, eval' ty< ^t >))

type LT<'m, 'n> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<LT< ^h, ^t >>, arg, _) =
    (<^)
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^h >, arg, eval' ty< ^h >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^t >, arg, eval' ty< ^t >))



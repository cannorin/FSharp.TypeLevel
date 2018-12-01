[<AutoOpen>]
module FSharp.TypeLevel.Natural
open TypeLevelOperators

type Z = struct end with
  static member inline Eval x = x
  static member inline LambdaOp (x, _) = x
  static member inline Add (_: ty<Z>, y) = eval' y

type S<'n> = struct end with
  static member inline Eval (_: ty<S< ^N >>) : _
    when ^N: (static member Eval: ty< ^N > -> ty< ^N' >) = ty<S< ^N' >>
  static member inline LambdaOp (_: ty<S< ^N >>, _: ^arg) : _
    when ^N: (static member LambdaOp: ty< ^N > * ^arg -> ty< ^N' >) = ty<S< ^N' >>
  static member inline Add (_: ty<S< ^X >>, _: ty< ^Y >) : _
    when ^X: (static member Eval: ty< ^X > -> ty< ^X' >)
     and ^X': (static member Add: ty< ^X' > * ty< ^Y > -> ty< ^Z >) =
    ty<S< ^Z >>

type Add<'x, 'y> = struct end with
  static member inline Eval (_: ty<Add< ^X, ^Y>>) : _
    when ^X: (static member Eval: ty< ^X > -> ty< ^X' >) =
    (^X': (static member Add: _*_ -> _) ty< ^X' >,ty< ^Y >)

type Z with
  static member inline Eq (_: ty<Z>, _: ty<Z>) = true'
  static member inline Eq (_: ty<Z>, _: ty<S<_>>) = false'
  static member inline Eq (_: ty<S<_>>, _: ty<Z>) = false'
  static member inline Sub (x, _: ty<Z>) = eval' x

type S<'n> with
  static member inline Eq (_: ty<S< ^M >>, _: ty<S< ^N >>) =
    ((^M or ^N): (static member Eq: ty< ^M > * ty< ^N > -> ty< ^Bool >) ty< ^M >, ty< ^N >)
  static member inline Sub (_: ty<S< ^X> >, _: ty<S< ^Y >>) : _
    when ^X: (static member Eval: ty< ^X > -> ty< ^X' >)
     and ^Y: (static member Eval: ty< ^Y > -> ty< ^Y' >)
     and ^Y': (static member Sub: ty< ^X' > * ty< ^Y' > -> ty< ^Z >) =
    ty< ^Z >

type Sub<'x, 'y> = struct end with
  static member inline Eval (_: ty<Sub< ^X, ^Y>>) : _
    when ^X: (static member Eval: ty< ^X > -> ty< ^X' >)
     and ^Y: (static member Eval: ty< ^Y > -> ty< ^Y' >) =
    (^Y': (static member Sub: _*_ -> _) ty< ^X' >,ty< ^Y' >)

type Z with
  static member inline Gt (_: ty<S<_>>, _: ty<Z>) = ty<True>
  static member inline Gt (_: ty<Z>, _) = ty<False>

type S<'n> with
  static member inline Gt (_: ty<S< ^X >>, _: ty<S< ^Y >>) : _
    when ^X: (static member Eval: ty< ^X > -> ty< ^X' >)
     and ^Y: (static member Eval: ty< ^Y > -> ty< ^Y' >)
     and (^X' or ^Y'): (static member Gt: ty< ^X' > * ty< ^Y' > -> ty< ^Z >) =
    ty< ^Z >

type GT<'x, 'y> = struct end with
  static member inline Eval (_: ty<GT< ^X, ^Y>>) : _
    when ^X: (static member Eval: ty< ^X > -> ty< ^X' >)
     and ^Y: (static member Eval: ty< ^Y > -> ty< ^Y' >) =
    ((^X' or ^Y'): (static member Gt: _*_ -> _) ty< ^X' >,ty< ^Y' >)

type LT<'x, 'y> = struct end with
  static member inline Eval (_: ty<LT< ^X, ^Y>>) : _
    when ^X: (static member Eval: ty< ^X > -> ty< ^X' >)
     and ^Y: (static member Eval: ty< ^Y > -> ty< ^Y' >) =
    ((^X' or ^Y'): (static member Gt: _*_ -> _) ty< ^Y' >,ty< ^X' >)

type GTE<'x, 'y> = Or<GT<'x, 'y>, Eq<'x, 'y>>
type LTE<'x, 'y> = Or<LT<'x, 'y>, Eq<'x, 'y>>

module TypeLevelOperators =
  let Z' = ty<Z>
  let inline S' (_: ty< ^N >) = ty<S< ^N >>
  let inline (+^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<Add< ^M, ^N >>
  let inline (-^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<Sub< ^M, ^N >>
  let inline (<^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<LT< ^M, ^N >>
  let inline (>^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<GT< ^M, ^N >>
  let inline (<=^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<LTE< ^M, ^N>>
  let inline (>=^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<GTE< ^M, ^N>>

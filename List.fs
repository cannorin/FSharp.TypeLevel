[<AutoOpen>]
module FSharp.TypeLevel.List
open FSharp.TypeLevel.Natural
open TypeLevelOperators

type Nil = struct end with
  static member inline Eval x = x
  static member inline LambdaOp (x, _) = x

type Cons<'a, 'b> = struct end with
  static member inline Eval (_: ty<Cons< ^h, ^t>>) : _
    when ^h: (static member Eval: ty< ^h > -> ty< ^H >)
     and ^t: (static member Eval: ty< ^t > -> ty< ^T >) = ty<Cons< ^H, ^T >>
  static member inline LambdaOp (_: ty<Cons< ^h, ^t>>, _: ^arg) : _
    when ^h: (static member LambdaOp: ty< ^h > * ^arg -> ty< ^H >)
     and ^t: (static member LambdaOp: ty< ^t > * ^arg -> ty< ^T >) = ty<Cons< ^H, ^T >>

module TypeLevelOperators =
  let nil' = ty<Nil>
  let inline cons' (x: ty< ^x >) (y: ty< ^y >) = ty<Cons< ^x, ^y >>
open TypeLevelOperators

type Nil with
  static member inline Eq (_: ty<Nil>, _: ty<Nil>) = true'
  static member inline Append (_: ty<Nil>, _: ty<'list>) = ty<'list>
  static member inline Contains (_: ty<Nil>, _: ty<_>) = false'
  static member inline Map (self: ty<Nil>, _) = self

type Cons<'a, 'b> with
  static member inline Eq (_: ty<Cons< ^h1, ^t1 >>, _: ty<Cons< ^h2, ^t2>>) =
     (eval' ty< ^h1 > =^ eval' ty< ^h2 >) &&^ (eval' ty< ^t1 > =^ eval' ty< ^t2 >)
  static member inline Eq (_: ty<Nil>, _:ty<Cons<_,_>>) = false'
  static member inline Eq (_: ty<Cons<_,_>>, _: ty<Nil>) = false'
  static member inline Append (_: ty<Cons<'h, ^t>>, _: ty<'list>) : _
    when ^t: (static member Append: ty< ^t > * ty<'list> -> ty<'newlist>) = ty<Cons<'h, 'newlist>>
  static member inline Contains (_: ty<Cons< ^h, ^t >>, _: ty< ^item >) : ty< ^Bool > =
    (^t: (static member Contains: ty< ^t > * ty< ^item > -> ty< ^Bool2 >) ty< ^t>, ty< ^item >) ||^ (ty< ^h > =^ ty< ^item >)
  static member inline Map (_: ty<Cons< ^h, ^t >>, f: ty< ^f >) =
    cons' (app' f ty< ^h >) ((^t or ^h): (static member Map: _*_ -> _) ty< ^t >, ty< ^f >)

module LIST =
  let EMPTY = ty<Nil>
  let inline CONS (_: ty< ^item >) (_: ty< ^list >) = eval' ty<Cons< ^item, ^list >>
  let inline CONTAINS (item: ty< ^item >) (xs: ty< ^list >) =
    (^list: (static member Contains: ty< ^list > * _ -> _) xs, eval' item)
  let inline APPEND (xs: ty< ^xs >) (ys: ty< ^ys >) =
    (^xs: (static member Append: _*_->_) xs,ys)
  let inline HEAD (_: ty<Cons<'h, _>>) = ty<'h>
  let inline TAIL (_: ty<Cons<_, 't>>) = ty<'t>
  let inline ISEMPTY xs = xs =^ EMPTY
  let inline MAP (f: ty< ^f >) (xs: ty< ^list >) =
    (^list: (static member Map: _*_ -> ty< ^list'>) xs,f)

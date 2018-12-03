[<AutoOpen>]
module FSharp.TypeLevel.Abstractions
open TypeLevelOperators

[<RequireQualifiedAccess>]
module Kind =
  type Abstract = interface inherit Kind.NotLambdaExpr end

[<RequireQualifiedAccess>]
module KindAbstract =
  type Functor = Functor with interface Kind.Abstract
  type Semigroup = Semigroup with interface Kind.Abstract
  type Foldable = Foldable with interface Kind.Abstract
  type Op = interface end

[<RequireQualifiedAccess>]
module FunctorOp =
  type Map = Map with interface KindAbstract.Op

[<RequireQualifiedAccess>]
module SemigroupOp =
  type Add = Add with interface KindAbstract.Op

[<RequireQualifiedAccess>]
module FoldableOp =
  type Fold = Fold with interface KindAbstract.Op

type Add<'left, 'right> =
  static member inline Eval (_: ty<Add< ^x, ^y >>) =
    let x : ty< ^X >= eval' ty< ^x >
    Kind.op (KindAbstract.Semigroup, SemigroupOp.Add, x, ty< ^y >, ty<Add< ^X, ^y >>)

type Map<'mapper, 'item> =
  static member inline Eval (_: ty<Map< ^f, ^x >>) =
    let f : ty< ^F > = eval' ty< ^f >
    let x : ty< ^X > = eval' ty< ^x >
    Kind.op (KindAbstract.Functor, FunctorOp.Map, x, f, ty<Map< ^F, ^X>>)

type Fold<'folder, 'init, 'item> =
  static member inline Eval (_: ty<Fold< ^f, ^i, ^x>>) =
    let f : ty< ^F > = eval' ty< ^f >
    let i : ty< ^I > = eval' ty< ^i >
    let x : ty< ^X > = eval' ty< ^x >
    Kind.op (KindAbstract.Foldable, FoldableOp.Fold, x, (i, f), ty<Fold< ^F, ^I, ^X>>)
 
module TypeLevelOperators =
  let inline (+^) (_: ty< ^M >) (_: ty< ^N >) = eval' ty<Add< ^M, ^N >>
  let inline map' (_: ty< ^f >) (_: ty< ^x >) = eval' ty<Map< ^f, ^x>>
  let inline fold' (_: ty< ^f >) (_: ty< ^init >) (_: ty< ^x >) = eval' ty<Fold< ^f, ^init, ^x>>
open TypeLevelOperators

type Add<'left, 'right> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<Add< ^h, ^t >>, arg, _) =
    (+^)
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^h >, arg, eval' ty< ^h >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^t >, arg, eval' ty< ^t >))

type Map<'left, 'right> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<Map< ^h, ^t >>, arg, _) =
    map'
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^h >, arg, eval' ty< ^h >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^t >, arg, eval' ty< ^t >))

type Fold<'f, 'i, 'x> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<Fold< ^f, ^i, ^x >>, arg, _) =
    fold'
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^f >, arg, eval' ty< ^f >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^i >, arg, eval' ty< ^i >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^x >, arg, eval' ty< ^x >))


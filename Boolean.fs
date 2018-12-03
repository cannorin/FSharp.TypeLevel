[<AutoOpen>]
module FSharp.TypeLevel.Boolean
open TypeLevelOperators

type True =
  static member inline Kind = Kind.Bool
  static member inline Eval self = self

type False =
  static member inline Kind = Kind.Bool
  static member inline Eval self = self

[<RequireQualifiedAccess>]
module BoolOp =
  type IfThenElse = IfThenElse

type True with
  static member inline Op (Kind.LambdaExpr, _, _, _, d) = d
  static member inline Op (Kind.Bool, Op.Eq, _:ty<True>, _:ty<True>, _) = ty<True>
  static member inline Op (Kind.Bool, Op.Eq, _:ty<True>, _:ty<False>, _) = ty<False>
  static member inline Op (Kind.Bool, BoolOp.IfThenElse, _: ty<True>, (l, r), _) = eval' l

type False with
  static member inline Op (Kind.LambdaExpr, _, _, _, d) = d
  static member inline Op (Kind.Bool, Op.Eq, _:ty<False>, _:ty<True>, _) = ty<False>
  static member inline Op (Kind.Bool, Op.Eq, _:ty<False>, _:ty<False>, _) = ty<True>
  static member inline Op (Kind.Bool, BoolOp.IfThenElse, _: ty<False>, (l, r), _) = eval' r

type IfThenElse<'condition, 'left, 'right> =
  static member inline Eval (_: ty<IfThenElse< ^c, ^l, ^r >>) : _
    when ^c: (static member Eval: ty< ^c > -> ty< ^C >) =
    Kind.op (
      Kind.Bool,
      BoolOp.IfThenElse,
      ty< ^C >,
      (ty< ^l >, ty< ^r >),
      ty<IfThenElse< ^C, ^l, ^r >>
    )

type Not<'bool> =
  static member inline Eval (_: ty<Not< ^b >>) : ty< ^b' > =
    eval' ty<IfThenElse< ^b, False, True >>

type And<'left, 'right> =
  static member inline Eval (_: ty<And< ^l, ^r >>) =
    eval' ty<IfThenElse< ^l, ^r, False>>

type Or<'left, 'right> =
  static member inline Eval (_: ty<Or< ^l, ^r >>) =
    eval' ty<IfThenElse< ^l, True, ^r>>

module TypeLevelOperators =
  let inline assert' (_: ty< ^X >) : unit
    when ^X: (static member Eval: ty< ^X > -> ty<True>) = ()
  let true' = ty<True>
  let false' = ty<False>
  let inline ifthenelse' (_: ty< ^cond >) (_: ty< ^l >) (_: ty< ^r >) =
    eval' ty<IfThenElse< ^cond, ^l, ^r>>
  let inline not' (_: ty< ^x >) = eval' ty<Not< ^x >>
  let inline (&&^) (_: ty< ^x >) (_: ty< ^y >) = eval' ty<And< ^x, ^y >>
  let inline (||^) (_: ty< ^x >) (_: ty< ^y >) = eval' ty<Or< ^x, ^y >>
open TypeLevelOperators

type IfThenElse<'c, 'a, 'b> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<IfThenElse< ^c, ^a, ^b >>, arg, _) =
    ifthenelse'
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^c >, arg, eval' ty< ^c >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^a >, arg, eval' ty< ^a >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^b >, arg, eval' ty< ^b >))

type Not<'b> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<Not< ^b >>, arg, _) =
    not'
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^b >, arg, eval' ty< ^b >))

type And<'a, 'b> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<And< ^a, ^b >>, arg, _) =
    (&&^)
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^a >, arg, eval' ty< ^a >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^b >, arg, eval' ty< ^b >))

type Or<'a, 'b> with
  static member inline Op (Kind.LambdaExpr, op, _: ty<Or< ^a, ^b >>, arg, _) =
    (||^)
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^a >, arg, eval' ty< ^a >))
      (Kind.op (Kind.LambdaExpr, op, eval' ty< ^b >, arg, eval' ty< ^b >))

[<AutoOpen>]
module FSharp.TypeLevel.Unit
open TypeLevelOperators

type Unit =
  static member inline Kind = Kind.Unit
  static member inline Eval x = x
  static member inline Op (Kind.LambdaExpr, _, _, _, d) = d
  static member inline Op (Kind.Unit, Op.Eq, _: ty<Unit>, _: ty<Unit>, _) = true'

type Ignore<'t> =
  static member inline Kind = Kind.Unit
  static member inline Eval _ = ty<Unit>
  static member inline Op (Kind.LambdaExpr, _, _, _, d) = d
  static member inline Op (Kind.Unit, Op.Eq, _: ty<Ignore<_>>, _: ty<Ignore<_>>, _) = true'

module TypeLevelOperators =
  let unit' = ty<Unit>
  let inline ignore' (_: ty<'t>) = ty<Unit>
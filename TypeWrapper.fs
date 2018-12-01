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

type Unit = struct end with
  static member inline Eval x = x
  static member inline LambdaOp (x, _) = x

type Const<'t> = struct end with
  static member inline Eval x = x
  static member inline LambdaOp (x, _) = x

module TypeLevelOperators =
  let inline eval' (x: ty< ^A >) : ty< ^B > =
    (^A: (static member Eval: ty< ^A > -> ty< ^B >) x)
 
type ty<'t> with
  static member inline (=^) (_: ty< ^A >, _: ty< ^B >) : ty< ^Bool >
    when ^A: (static member Eval: ty< ^A > -> ty< ^A' >)
     and ^B: (static member Eval: ty< ^B > -> ty< ^B' >) =
    ((^A' or ^B'): (static member Eq: ty< ^A' > * ty< ^B' > -> ty< ^Bool >) ty< ^A' >(), ty< ^B' >())
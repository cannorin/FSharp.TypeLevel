[<AutoOpen>]
module FSharp.TypeLevel.Boolean
open FSharp.TypeLevel.TypeWrapper.TypeLevelOperators

type True = struct end with
  static member inline Eval x = x
  static member inline LambdaOp (x, _) = x
  static member inline IfThenElse (_: ty<True>, x, y) = x

type False = struct end with
  static member inline Eval x = x
  static member inline LambdaOp (x, _) = x
  static member inline IfThenElse (_: ty<False>, x, y) = y 

type True with
  static member inline Eq (_: ty<True>, _: ty<True>) = ty<True>

type False with
  static member inline Eq (_: ty<True>, _: ty<False>) = ty<False>
  static member inline Eq (_: ty<False>, _: ty<False>) = ty<True>
  static member inline Eq (_: ty<False>, _: ty<True>) = ty<False>

type Assert< ^X when ^X: (static member Eval: ty< ^X > -> ty<True>) > = struct end with
  static member inline Eval (_: ty<Assert<_>>) = ty<Unit>

type Not<'a> = struct end with
  static member inline Eval (_: ty<Not< ^A >>) : ty< ^B >
    when ^A: (static member Eval: ty< ^A > -> ty< ^Bool >) =
    (^Bool: (static member IfThenElse: ty< ^Bool > * _ * _ -> ty< ^B >) ty< ^Bool >, ty<False>, ty<True>)

type And<'a, 'b> = struct end with
  static member inline Eval (_: ty<And< ^A, ^B >>) : ty< ^C >
    when ^A: (static member Eval: ty< ^A > -> ty< ^A' >)
     and ^B: (static member Eval: ty< ^B > -> ty< ^B' >) =
    (^A': (static member IfThenElse: _*_*_ -> ty< ^C >) ty< ^A' >,ty< ^B' >,ty<False>)

type Or<'a, 'b>  = struct end with
  static member inline Eval (_: ty<Or< ^A, ^B >>) : ty< ^C >
    when ^A: (static member Eval: ty< ^A > -> ty< ^A' >)
     and ^B: (static member Eval: ty< ^B > -> ty< ^B' >) =
    (^A': (static member IfThenElse: _*_*_ -> ty< ^C >) ty< ^A' >,ty<True>,ty< ^B' >)

type IfThenElse<'_bool, 'a, 'b> = struct end with
  static member inline Eval (_: ty<IfThenElse< ^X, ^A, ^B>>) : ty< ^EvalAorB >
    when ^X: (static member Eval: ty< ^X > -> ty< ^Bool >) =
    (^Bool: (static member IfThenElse: _*_*_ -> ty< ^EvalAorB >) ty< ^Bool >,eval' ty< ^A >,eval' ty< ^B >)

type Eq<'a, 'b> = struct end with
  static member inline Eval (_: ty<Eq< ^A, ^B >>) : ty< ^Bool > = ty< ^A > =^ ty< ^B >

module TypeLevelOperators =
  let inline assert' (_: ty< ^X >) : unit
    when ^X: (static member Eval: ty< ^X > -> ty<True>) = ()
  let true' = ty<True>
  let false' = ty<False>
  let inline not' (_: ty< ^a >) = eval' ty<Not< ^a >>
  let inline ifthenelse' (_: ty< ^cond >) (_: ty< ^a >) (_: ty< ^b >) =
    eval' ty<IfThenElse< ^cond, ^a, ^b >>
  let inline (&&^) (_: ty< ^a >) (_: ty< ^b >) = eval' ty<And< ^a, ^b>>
  let inline (||^) (_: ty< ^a >) (_: ty< ^b >) = eval' ty<Or< ^a, ^b>>

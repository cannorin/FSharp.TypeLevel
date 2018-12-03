[<AutoOpen>]
module FSharp.TypeLevel.Lambda
open FSharp.TypeLevel
open TypeLevelOperators

module LambdaOp =
  type Subst = Subst
  type Shift = Shift

  let inline lambdaop op (x: ty< ^x >) arg =
    Kind.op (Kind.LambdaExpr, op, eval' x, arg, eval' x)

  let inline subst var value x = lambdaop Subst x (Z', (var, value))
  let inline shift isAdd d x =   lambdaop Shift x (Z', (isAdd, d))

open LambdaOp

type Var<'nat> =
  static member inline Kind = Kind.LambdaExpr
type Lam<'term> =
  static member inline Kind = Kind.LambdaExpr
type App<'term1, 'term2> =
  static member inline Kind = Kind.LambdaExpr

let inline private var' (_: ty< ^n>) = ty<Var< ^n>>
let inline private lam' (_: ty< ^t>) = ty<Lam< ^t>>
let inline private app' (_: ty< ^a>, _: ty< ^b>) = ty<App< ^a, ^b>>

let inline private ifte (_: ty< ^c >) (_: ty< ^l >) (_: ty< ^r >) = ty<IfThenElse< ^c, ^l, ^r >>
let inline private lt (x: ty< ^x >) (y: ty< ^y >) = ty<LT< ^x, ^y >>
let inline private eq (x: ty< ^x >) (y: ty< ^y >) = ty<Eq< ^x, ^y >>
let inline private add (x: ty< ^x >) (y: ty< ^y >) = ty<Add< ^x, ^y >>

type Var<'nat> with
  static member inline Eval (_: ty<Var< ^n >>) = var' (eval' ty< ^n >)
  static member inline Op (Kind.Bool, _, _, _, d) = d
  static member inline Op (Kind.Nat, _, _, _, d) = d
  static member inline Op (Kind.List, _, _, _, d) = d
  static member inline Op (Kind.Unit, _, _, _, d) = d
  static member inline Op (KindAbstract.Functor, _, _, _, d) = d
  static member inline Op (KindAbstract.Foldable, _, _, _, d) = d
  static member inline Op (KindAbstract.Semigroup, _, _, _, d) = d
  static member inline Op (Kind.LambdaExpr, Op.Eq, _: ty<Var< ^n >>, _: ty<Var< ^m >>, _) =
    ty< ^n > =^ ty< ^m >
  static member inline Op (Kind.LambdaExpr, Subst, self: ty<Var< ^n >>, (c, (var, value)), _) =
    let n = ty< ^n >
    ifte (eq (add c var) n)
      (shift true' c value)
      self
  static member inline Op (Kind.LambdaExpr, Shift, self: ty<Var< ^n >>, (c, (_: ty<True>, d)), _) =
    let n = ty< ^n >
    ifte (lt n c)
      self
      (var' (add n d))
  static member inline Op (Kind.LambdaExpr, Shift, self: ty<Var<Z>>, (_, (_: ty<False>, _)), _) = self
  static member inline Op (Kind.LambdaExpr, Shift, self: ty<Var<S< ^n >>>, (c: ty< ^c >, (_: ty<False>, d: ty<S<Z>>)), _) =
    ifte (lt ty<S< ^n >> c)
      self
      (var' ty< ^n >)
  static member inline Apply (l: ty<Var< ^n >>, r: ty< ^x >) = app' (l ,eval' r)

type Lam<'term> with
  static member inline Eval (_: ty<Lam< ^t >>) = lam' (eval' ty< ^t >)
  
  static member inline Op (Kind.LambdaExpr, Subst, _: ty<Lam< ^t >>, (c, arg), _) =
    lam' (lambdaop Subst ty< ^t > (S' c, arg))

  static member inline Op (Kind.LambdaExpr, Shift, _: ty<Lam< ^t >>, (c, arg), _) =
    lam' (lambdaop Shift ty< ^t > (S' c, arg))

  static member inline Op (Kind.LambdaExpr, Op.Eq, _: ty<Lam< ^t >>, _: ty<Lam< ^u >>, _) =
    ty< ^t > =^ ty< ^u >

  static member inline Apply (l: ty<Lam< ^b >>, r: ty< ^x >) =
    shift false' (S' Z')
      (subst Z'
        (shift true' (S' Z') r)
        ty< ^b >)
    |> eval'

type Let<'value, 'body> = App<Lam<'body>, 'value>

let inline private apply (l: ty< ^l >, r: ty< ^r >) =
  ((^l or ^r): (static member Apply: ty< ^l > * ty< ^r > -> ty< ^result >) l,r)

type App<'term1, 'term2> with
  static member inline Op (Kind.Bool, _, _, _, d) = d
  static member inline Op (Kind.Nat, _, _, _, d) = d
  static member inline Op (Kind.List, _, _, _, d) = d
  static member inline Op (Kind.Unit, _, _, _, d) = d
  static member inline Op (KindAbstract.Functor, _, _, _, d) = d
  static member inline Op (KindAbstract.Foldable, _, _, _, d) = d
  static member inline Op (KindAbstract.Semigroup, _, _, _, d) = d
  static member inline Op (Kind.LambdaExpr, Subst, self: ty<App< ^t1, ^t2 >>, arg: 'arg, _) : _
    when (^t1 or ^t2): (static member Op: Kind.LambdaExpr * Subst * ty< ^t1 > * 'arg * _ -> ty< ^T1 >)
     and (^t2 or ^t1): (static member Op: Kind.LambdaExpr * Subst * ty< ^t2 > * 'arg * _ -> ty< ^T2 >) = ty<App< ^T1, ^T2 >>
  
  static member inline Op (Kind.LambdaExpr, Shift, self: ty<App< ^t1, ^t2 >>, arg: 'arg, _) : _
    when (^t1 or ^t2): (static member Op: Kind.LambdaExpr * Shift * ty< ^t1 > * 'arg * _ -> ty< ^T1 >)
     and (^t2 or ^t1): (static member Op: Kind.LambdaExpr * Shift * ty< ^t2 > * 'arg * _ -> ty< ^T2 >) = ty<App< ^T1, ^T2 >>
  
  static member inline Op (Kind.LambdaExpr, Op.Eq, _: ty<App< ^t11, ^t12 >>, _: ty<App< ^t21, ^t22 >>, _) =
    ty< ^t11 > =^ ty< ^t21 > &&^ ty< ^t12 > =^ ty< ^t22 >

  static member inline Eval (_: ty<App< ^l, ^r >>) =
    apply (eval' ty< ^l >, ty< ^r >)
  static member inline Apply (l: ty<App<_, _>>, r: ty< ^x >) =
    app' (l, eval' r)

module TypeLevelOperators =
  let inline var' (_: ty<'n>) = eval' ty<Var<'n>>
  let inline lam' (_: ty<'t>) = eval' ty<Lam<'t>>
  let inline app' (f: ty< ^f >) (x: ty< ^x >) = apply (eval' f, x)
  let inline let' (_: ty< ^value >) (_: ty< ^body >) = eval' ty<Let< ^value, ^body >>
  let inline (|>^) (x: ty< ^x >) (f: ty< ^f >) = app' f x
  let inline (<|^) (f: ty< ^f >) (x: ty< ^x >) = app' f x 
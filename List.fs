[<AutoOpen>]
module FSharp.TypeLevel.List
open TypeLevelOperators

type Nil =
  static member inline Kind = Kind.List
  static member inline Eval x = x

type Cons<'head, 'tail> =
  static member inline Kind = Kind.List
  static member inline Eval (_: ty<Cons< ^h, ^t>>) : _
    when ^h: (static member Eval: ty< ^h > -> ty< ^H >)
     and ^t: (static member Eval: ty< ^t > -> ty< ^T >) = ty<Cons< ^H, ^T >>

module TypeLevelOperators =
  let nil' = ty<Nil>
  let inline cons' (x: ty< ^x >) (y: ty< ^y >) = ty<Cons< ^x, ^y >>
open TypeLevelOperators

type Nil with 
  static member inline Op (Kind.List, Op.Eq, _: ty<Nil>, _: ty<Nil>, _) = true'
  static member inline Op (Kind.List, Op.Eq, _: ty<Nil>, _: ty<Cons<_, _>>, _) = false'
  static member inline Op (Kind.LambdaExpr, _, _, _, d) = d
  static member inline Op (KindAbstract.Functor, FunctorOp.Map, self, _, _) = self
  static member inline Op (KindAbstract.Semigroup, SemigroupOp.Add, _: ty<Nil>, x, _) = x
  static member inline Op (KindAbstract.Foldable, FoldableOp.Fold, _, (init, _), _) = init

type Cons<'head, 'tail> with
  static member inline Op (Kind.List, Op.Eq, _: ty<Cons<_, _>>, _: ty<Nil>, _) = false'
  static member inline Op (Kind.List, Op.Eq, _: ty<Cons< ^h1, ^t1>>, _: ty<Cons< ^h2, ^t2 >>, _) =
    ty< ^h1 > =^ ty< ^h2 > &&^ ty< ^t1 > =^ ty< ^t2 >
  static member inline Op (Kind.LambdaExpr, op, _: ty<Cons< ^h, ^t >>, arg, _) =
    cons'
      (Kind.op (Kind.LambdaExpr, op, ty< ^h >, arg, ty< ^h >))
      (Kind.op (Kind.LambdaExpr, op, ty< ^t >, arg, ty< ^t >))
  static member inline Op (KindAbstract.Functor, FunctorOp.Map, _: ty<Cons< ^h, ^t>>, f: ty< ^f >, _) =
    cons' (app' f ty< ^h >) (map' f ty< ^t >)
  static member inline Op (KindAbstract.Semigroup, SemigroupOp.Add, _: ty<Cons< ^h, ^t >>, _: ty< ^list >, _) =
    cons' ty< ^h > (ty< ^t > +^ ty< ^list>)
  static member inline Op (KindAbstract.Foldable, FoldableOp.Fold, _: ty<Cons< ^h, ^t >>, (i: ty< ^init >, f: ty< ^f >), _) =
    fold' f (app' (app' f i) ty< ^h >) ty< ^t >

module TypeList =
  type IsEmpty<'list> = Eq<'list, Nil>
  type ForAll<'predicate, 'list> =
    Fold<
      Lam<Lam<And<Var<S<Z>>, Var<Z>>>>,
      True,
      Map<'predicate, 'list>>
  type Exists<'predicate, 'list> =
    Fold<
      Lam<Lam<Or<Var<S<Z>>, Var<Z>>>>,
      False,
      Map<'predicate, 'list>>
  type Contains<'item, 'list> =
    Exists<
      Lam<Eq<'item, Var<Z>>>,
      'list>
  type Concat<'list> =
    Fold<
      Lam<Lam<Add<Var<S<Z>>, Var<Z>>>>,
      Nil,
      'list>
  type Filter<'predicate, 'list> =
    Concat<
      Map<
        Lam<
          IfThenElse<
            App<'predicate, Var<Z>>,
            Cons<Var<Z>, Nil>,
            Nil>>,
        'list>>
  type IsSubset<'sublist, 'list> =
    ForAll<
      Lam<Contains<Var<Z>, 'list>>,
      'sublist>
  type SetEquals<'xs, 'ys> =
    And<IsSubset<'xs, 'ys>, IsSubset<'ys, 'xs>>

  let inline isEmpty xs = xs =^ nil'
  let inline append (_: ty< ^xs >) (_: ty< ^ys >) = ty< ^xs > +^ ty< ^ys >
  let inline forall (_: ty< ^predicate >) (_: ty< ^xs >) = eval' ty<ForAll< ^predicate, ^xs >>
  let inline exists (_: ty< ^predicate >) (_: ty< ^xs >) = eval' ty<Exists< ^predicate, ^xs >>
  let inline contains (_: ty< ^i >) (_: ty< ^xs >) : _
    when ^i : (static member Eval: ty< ^i > -> ty< ^I >) = eval' ty<Contains< ^I, ^xs>>
  let inline concat (_: ty< ^xs >) = eval' ty<Concat< ^xs >>
  let inline filter (_: ty<'predicate>) (_: ty<'xs>) = eval' ty<Filter< 'predicate, 'xs >>
  let inline isSubset (_: ty<'sublist>) (_: ty<'list>) = eval' ty<IsSubset<'sublist,  'list>>
  let inline setEquals (_: ty<'xs>) (_: ty<'ys>) = eval' ty<SetEquals<'xs, 'ys>>
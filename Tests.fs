module FSharp.TypeLevel.Tests
open FSharp.TypeLevel
open TypeLevelOperators

assert' (not' (not' true'))
assert' ((not' true' ||^ true') &&^ not' false')

let one = S' Z'
let three = one +^ one +^ one
let five  = S' (three +^ one)
let two   = three -^ one
let six   = five +^ two -^ one

assert' (six =^ three +^ three)
assert' (not' (six >^ three &&^ five -^ one <=^ two +^ one))

let S =
  // \x \y \z (xz)(yz)
  lam' (lam' (lam' (app' (app' (var' two) (var' Z')) (app' (var' one) (var' Z')))))
let K =
  // \x \y x
  lam' (lam' (var' (S' Z')))

let I  = lam' (var' Z')
let I2 = app' (app' S K) K
assert' (I =^ I2)

let T = K
let F = S <|^ K

let NOT =
  S <|^ (S <|^ I <|^ (K <|^ F)) <|^ (K <|^ T)

let notnotT = T |>^ NOT |>^ NOT
assert' (notnotT =^ T)

let seven =
  let' five (
    let' two (
      var' Z' +^ var' one ))
assert' (seven =^ one +^ six)

let xs =
  nil' |> cons' (S' Z')
       |> cons' (Z' +^ Z')
       |> cons' (S' (S' Z'))

let xsSum =
  xs |> fold' (lam' (lam' (var' Z' +^ var' one))) Z'
assert' (xsSum =^ three)

let xs' =
  xs |> map' (lam' (S' (S' Z') +^ var' Z'))

let ys =
  nil' |> cons' (S' (S' Z') +^ S' Z')
       |> cons' (S' Z' -^ S' Z')
       |> cons' (S' Z' +^ S' Z' +^ S' Z')
       |> cons' (S' Z' +^ S' Z')

let zs =
  TypeList.append xs ys

let zsContainsZero = zs |> TypeList.contains Z'
assert' zsContainsZero

assert' (
  TypeList.forall (lam' (var' Z' >^ two)) zs |> not'
)

assert' (
  TypeList.exists (lam' (var' Z' >^ two)) zs
)

let ws = nil' |> cons' xs |> cons' xs'
let ws' = TypeList.concat ws

// assert' (TypeList.isSubset xs ws')

assert' (
  // xs |> TypeList.forall (lam' (ws' |> TypeList.contains (var' Z')))
  let predicate = lam' (ws' |> TypeList.contains (var' Z'))
  xs |> TypeList.forall predicate
)

//let vs = TypeList.filter (lam' (var' Z' >^ two)) zs
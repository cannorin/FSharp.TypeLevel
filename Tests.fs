module private FSharp.TypeLevel.Tests
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

let I = lam' (var' Z')
let I2 = app' (app' S K) K

let T = K
let F = S <|^ K

let NOT =
  S <|^ (S <|^ I <|^ (K <|^ F)) <|^ (K <|^ T)

let x = T |>^ NOT |>^ NOT

let xs =
  nil' |> cons' (S' Z')
       |> cons' (Z' +^ Z')
       |> cons' (S' (S' Z'))

let xs' =
  let f = lam' (S' (S' Z') +^ var' Z')
  xs |> LIST.MAP f

let ys =
  nil' |> cons' (S' (S' Z') +^ S' Z')
       |> cons' (S' Z' -^ S' Z')
       |> cons' (S' Z' +^ S' Z' +^ S' Z')
       |> cons' (S' Z' +^ S' Z')

let zs =
  LIST.APPEND xs ys

let zsContainsZero = zs |> LIST.CONTAINS Z'
assert' zsContainsZero

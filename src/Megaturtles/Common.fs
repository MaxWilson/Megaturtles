[<AutoOpen>]
module Common

let thunk v _ = v
let thunk1 f x _ = f x
let thunk2 f x y = f x y
let flip f x y = f y x
module Tup2 =
    let create x y = x,y
    let createf f x y = (x,y) |> f
    let map1 f (x,y) = f x, y
    let map2 f (x,y) = x, f y
    let mapf f x y = (x,y) |> f
let tup2 = Tup2.create
[<AutoOpen>]
module Common

let notImpl v = failwith $"Not implemented. Email Max if you want this feature. {v}"
let shouldntHappen v = failwith $"Bug! This shouldn't ever happen even though the type system doesn't prevent it--it should be prevented by runtime logic."
let thunk v _ = v
let thunk1 f x _ = f x
let thunk2 f x y = f x y
let flip f x y = f y x
let rand = System.Random()
module String =
    let trim (s:string) = s.Trim()
    let trimEnd (s:string) = s.TrimEnd()
    let trimLinefeeds (s:string) = s.TrimEnd('\r')
module Tup2 =
    let create x y = x,y
    let createf f x y = (x,y) |> f
    let map1 f (x,y) = f x, y
    let map2 f (x,y) = x, f y
    let mapf f x y = (x,y) |> f
let tup2 = Tup2.create
module Array =
    let every f arr =
        arr |> Array.exists (not << f) |> not
module Array2 =
    let replace m n newContent array =
        array
        |> Array.mapi (fun m' row ->
            row |> Array.mapi (fun n' content ->
                    if m = m' && n = n' then newContent else content
                )
            )

#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"
#load "Execution.fs"
open Megaturtles.Execution

flip (-) 12 7
(tup2 3 4) |> Tup2.map1 ((+) 1)
[1;2;3] |> List.map (thunk2 (*) 3) |> List.mapi tup2 |> List.map (Tup2.map1 ((+) 1))
["a";"b";"c"] |> List.map (flip (+) " babble") |> List.mapi (Tup2.createf (Tup2.map1 ((+) 1)))

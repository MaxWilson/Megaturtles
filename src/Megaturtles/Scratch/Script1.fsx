#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"

flip (-) 12 7
(tup2 3 4) |> Tup2.map1 ((+) 1)
[1;2;3] |> List.map (thunk2 (*) 3) |> List.mapi tup2 |> List.map (Tup2.map1 ((+) 1))
["a";"b";"c"] |> List.map (flip (+) " babble") |> List.mapi (Tup2.createf (Tup2.map1 ((+) 1)))

type GridContent = Wall | Pit | Turtle of turtleNumber: int
type GridSetupContent = StartPosition of turtleNumber: int | Content of GridContent
type Grid = {
    cells: GridContent option[][]
    }
type Coords = int * int
type Direction = North | South | East | West
type Turtle = {
    id: int
    coords: Coords
    facing: Direction
    }
type World = {
    grid: Grid
    turtles: Map<int, Turtle>
    }
type TurnDirection = Right | Left | Random
type InstructionAddress = int
type Instruction =
    | Forward
    | Turn of TurnDirection
let createGrid (start: GridSetupContent option[][]) =
    {
    cells = (start |> Array.map (Array.map (function Some (StartPosition n) -> Turtle n |> Some | Some (Content c) -> Some c | _ -> None)))
    }
let createWorld start =
    let grid = createGrid start
    let turtles = [
        for m, row in grid.cells |> Array.mapi tup2 do
            for n, cell in row |> Array.mapi tup2 do
                match cell with
                | Some (Turtle id) ->
                    id, { coords = m,n; facing = North; id = id }
                | _ -> ()
        ]
    {
        grid = grid
        turtles = turtles |> Map.ofList
        }
let prettysPrint world =
    let grid = world.grid
    let s = new System.Text.StringBuilder()
    for row in grid.cells do
        for cell in row do
            match cell with
            | Some Wall -> s.Append "#"
            | Some Pit -> s.Append "o"
            | Some (Turtle n) -> s.Append (n.ToString())
            | None -> s.Append "."
            |> ignore
        s.Append "\n" |> ignore
    s.ToString()
let execute (turtleNumber:int, instruction) (world: World) : World * InstructionAddress option =
    match instruction with
    | Forward -> notImpl()
    | Turn d -> notImpl()
let textToStart (txt: string) =
    txt.Trim().Split("\n")
    |> Array.map (fun line ->
        [|  for c in line do
                let (|Number|_|) c =
                    match System.Int32.TryParse (c.ToString()) with
                    | true, n -> Some n
                    | _ -> None
                match c with
                | '#' -> Wall |> Content |> Some
                | 'o' -> Pit |> Content |> Some
                | Number n -> StartPosition n |> Some
                | _ -> None
            |]
        )

let scriptExecute n cmds world =
    // in this simple scenario we aren't concerned with instructions for goto
    cmds |> List.fold (fun world instruction -> execute(n, instruction) world |> fst) world

let print = prettysPrint >> printfn "%s"
"""
######
# # ##
# # o#
#ooo #
#    #
#1  2#
######
"""
|> textToStart |> createWorld
|> scriptExecute 1 [Forward] |> scriptExecute 2 [Forward; Turn Left; Forward]
|> print

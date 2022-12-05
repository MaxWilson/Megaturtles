#I __SOURCE_DIRECTORY__
#I ".."
#load "Common.fs"

flip (-) 12 7
(tup2 3 4) |> Tup2.map1 ((+) 1)
[1;2;3] |> List.map (thunk2 (*) 3) |> List.mapi tup2 |> List.map (Tup2.map1 ((+) 1))
["a";"b";"c"] |> List.map (flip (+) " babble") |> List.mapi (Tup2.createf (Tup2.map1 ((+) 1)))

exception Impossible of string
type GridContent = Wall | Pit | Turtle of turtleNumber: int
type GridSetupContent = StartPosition of turtleNumber: int | Content of GridContent
type Grid = GridContent option[][]
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
    (start |> Array.map (Array.map (function Some (StartPosition n) -> Turtle n |> Some | Some (Content c) -> Some c | _ -> None)))
let createWorld start =
    let grid = createGrid start
    let turtles = [
        for m, row in grid |> Array.mapi tup2 do
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
    for row in grid do
        for cell in row do
            match cell with
            | Some Wall -> s.Append "#"
            | Some Pit -> s.Append "o"
            | Some (Turtle n) -> s.Append (n.ToString())
            | None -> s.Append "."
            |> ignore
        s.Append "\n" |> ignore
    for t in world.turtles.Values do
        let dir = match t.facing with North -> "^" | South -> "v" | East -> ">" | West -> "<"
        s.AppendLine $"Turtle {t.id}: {dir}" |> ignore
    s.ToString()
let execute (turtleNumber:int, instruction) (world: World) : InstructionAddress option * World =
    match instruction with
    | Forward ->
        let turtle = world.turtles[turtleNumber]
        let m,n = turtle.coords
        let m',n' =
            match turtle.facing with
            | North -> m-1, n
            | South -> m+1, n
            | East -> m, n+1
            | West -> m, n-1
        match world.grid[m'][n'] with
        | None ->
            None, { world with
                        turtles = world.turtles |> Map.change turtleNumber (function Some t -> Some { t with coords = m', n' } | _ -> None)
                        grid = world.grid |> Array2.replace m n None |> Array2.replace m' n' (Some (Turtle turtleNumber)) }
        | Some _ -> Impossible $"Something is already there! {world.grid[m][n]}" |> raise
    | Turn d ->
        let turtle = world.turtles[turtleNumber]
        let facing' =
            let turn = match d with | (Left | Right) as f -> f | Random -> if rand.Next 2 = 0 then Left else Right
            match turtle.facing, turn with
            | North, Left | South, Right -> West
            | West, Left | East, Right -> South
            | South, Left | North, Right -> East
            | East, Left | West, Right -> North
            | _ -> shouldntHappen()
        None, { world with turtles = world.turtles |> Map.change turtleNumber (function Some t -> Some { t with facing = facing' } | None -> None) }

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
    cmds |> List.fold (fun world instruction -> execute(n, instruction) world |> snd) world

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

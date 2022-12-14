// execution layer for Megaturtles, basically the equivalent of IL. Language and UI compile down to this.
module Megaturtles.Execution

exception Impossible of string
type TurtleId = int
type GridContent = Wall | Pit | Victory
type GridSetupContent = StartPosition of TurtleId | Content of GridContent
type Grid = GridContent option[][]
type Coords = int * int
type Direction = North | South | East | West
type TurtleStatus = Active | Dead | Victor
type Turtle = {
    id: int
    coords: Coords
    facing: Direction
    status: TurtleStatus
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
    // note that a Jump command in the *language* turns into Jump + Forward instructions, where Jump is allowed to move into a Pit without dying but Forward is not. This also ensures
    // that it takes two tempo to Jump.
    | Jump
    | Shoot
let createGrid (start: GridSetupContent option[][]) =
    (start |> Array.map (Array.map (function Some (Content c) -> Some c | _ -> None)))
let createWorld start =
    let grid = createGrid start
    let turtles = [
        for m, row in start |> Array.mapi tup2 do
            for n, cell in row |> Array.mapi tup2 do
                match cell with
                | Some (StartPosition id) ->
                    id, { coords = m,n; facing = North; id = id; status = Active }
                | _ -> ()
        ]
    {
        grid = grid
        turtles = turtles |> Map.ofList
        }
let (|TurtleAt|_|) (m,n) world = (world.turtles.Values |> Seq.tryFind(fun t -> t.coords = (m,n)))

let prettysPrint world =
    let grid = world.grid
    let s = new System.Text.StringBuilder()
    for m, row in grid |> Array.mapi tup2 do
        for n, cell in row |> Array.mapi tup2 do
            match cell, world with
            | Some (Victory _), _ -> s.Append "$"
            | _, TurtleAt (m,n)  turtle -> s.Append (turtle.id.ToString())
            | Some Wall, _ -> s.Append "#"
            | Some Pit, _ -> s.Append "o"
            | None, _ -> s.Append "."
            |> ignore
        s.Append "\n" |> ignore
    for t in world.turtles.Values do
        let dir = match t.facing with North -> "^" | South -> "v" | East -> ">" | West -> "<"
        let status = match t.status with Active -> " " | Dead -> "[DEAD] " | Victor -> "[VICTOR!] "
        s.AppendLine $"Turtle {t.id}:{status} {dir} {t.coords}" |> ignore
    s.ToString()
let forward1 world (m,n) facing =
    match facing with
        | North -> m-1, n
        | South -> m+1, n
        | East -> m, n+1
        | West -> m, n-1
    |> fun (m, n) ->
        let between inclusiveMin exclusiveMax n = inclusiveMin <= n && n < exclusiveMax
        if m |> between 0 world.grid.Length && n |> between 0 world.grid[m].Length then
            Some (m, n)
        else None
let execute (turtleId:TurtleId, instruction) (world: World) : InstructionAddress option * World =
    let turtle = world.turtles[turtleId]
    match instruction with
    | _ when turtle.status = Dead || turtle.status = Victor -> None, world // ignore further instructions after death or victory
    | Forward | Jump ->
        let m,n = turtle.coords
        match forward1 world (m,n) turtle.facing with
        | None ->
            // turtle exits the map and dies
            None, { world with
                        turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with status = Dead} | _ -> None)
                        grid = world.grid |> Array2.replace m n None
                        }
        | Some(m', n') ->
            let move() =
                None, { world with
                            turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with coords = m', n' } | _ -> None)
                            }
            match world with
            | TurtleAt (m',n') otherTurtle when otherTurtle.status <> Victor ->
                // kill both turtles but don't move--there's no room
                None, { world with
                            turtles = world.turtles
                                        |> Map.change turtleId (function Some t -> Some { t with status = Dead } | _ -> None)
                                        |> Map.change otherTurtle.id (function Some t -> Some { t with status = Dead } | _ -> None)
                            }
            | _ ->
                match world.grid[m'][n'] with
                | None -> move()
                | Some(Pit) when instruction = Jump -> move()
                | Some (Wall | Pit) ->
                    // kill turtle
                    None, { world with
                                turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with coords = m', n'; status = Dead } | _ -> None)
                                grid = world.grid |> Array2.replace m n None // dead turtle body knocks down wall or fills in pit
                                }
                | Some Victory ->
                    // turtle exits the map victorious
                    None, { world with
                                turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with coords = m', n'; status = Victor } | _ -> None)
                                grid = world.grid |> Array2.replace m n None
                                }
    | Turn d ->
        let facing' =
            let turn = match d with | (Left | Right) as f -> f | Random -> if rand.Next 2 = 0 then Left else Right
            match turtle.facing, turn with
            | North, Left | South, Right -> West
            | West, Left | East, Right -> South
            | South, Left | North, Right -> East
            | East, Left | West, Right -> North
            | _ -> shouldntHappen()
        None, { world with turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with facing = facing' } | None -> None) }
    | Shoot ->
        let rec findTarget coords =
            match forward1 world coords turtle.facing with
            | None -> world // didn't hit anything
            | Some (m,n) ->
                // see if missile hits a turtle or a wall
                match world, world.grid[m][n] with
                | TurtleAt (m,n) target, _ ->
                    // kill the target turtle and turn its location into a crater
                    { world with
                                turtles = world.turtles |> Map.change target.id (function Some t -> Some { t with status = Dead } | _ -> None)
                                grid = world.grid |> Array2.replace m n (Some Pit) // dead turtle body knocks down wall or fills in pit
                                }
                | _, Some Wall ->
                    // turn wall into a crater
                    { world with
                                grid = world.grid |> Array2.replace m n (Some Pit) // dead turtle body knocks down wall or fills in pit
                                }
                | _ ->
                    // else move missile forward and re-check
                    findTarget (m,n)
        None, (findTarget turtle.coords)


let textToStart (txt: string) =
    let rows = txt.Trim().Split("\n") |> Array.map String.trimLinefeeds
    let n = (rows |> Array.map String.length |> Array.max)
    rows
    |> Array.map (fun line ->
        if line.Length < n then
            line + String.replicate (n - line.Length) " "
        else line
        )
    |> Array.map (fun line ->
        [|  for c in line do
                let (|Number|_|) c =
                    match System.Int32.TryParse (c.ToString()) with
                    | true, n -> Some n
                    | _ -> None
                match c with
                | '#' -> Wall |> Content |> Some
                | 'o' -> Pit |> Content |> Some
                | '$' -> Victory |> Content |> Some
                | Number n -> StartPosition n |> Some
                | _ -> None
            |]
        )

#if FABLE_COMPILER
#else
// .fsx support for Go To Definition is worse, so putting this test logic in the code for now to ease development.
let scriptExecute n cmds world =
    // in this simple scenario we aren't concerned with instructions for goto
    cmds |> List.fold (fun world instruction -> execute(n, instruction) world |> snd) world

let print = prettysPrint >> printfn "%s"
let interactive() =
    """
###$##
# # ##
# # o#
#ooo #
#    #
#1  2#
######
"""
    |> textToStart |> createWorld
    // note that a Jump instruction turns into Jump + Forward bytecode, where Jump is allowed to move into a Pit without dying but Forward is not. This also ensures
    // that it takes two tempo to Jump.
    |> scriptExecute 1 [Forward; Jump; Forward; Forward; Turn Right; Shoot; Jump; Forward; Turn Left; Forward] |> scriptExecute 2 [Forward; Turn Left; Forward; Turn Right; Jump; Forward; Forward; Forward]
    |> print
#endif
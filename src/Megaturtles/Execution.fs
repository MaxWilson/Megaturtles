// execution layer for Megaturtles, basically the equivalent of IL. Language and UI compile down to this.
module Megaturtles.Execution

exception Impossible of string
type TurtleId = int
type GridContent = Wall | Pit | Turtle of TurtleId | Victory
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
    | Jump
    | Shoot
let createGrid (start: GridSetupContent option[][]) =
    (start |> Array.map (Array.map (function Some (StartPosition n) -> Turtle n |> Some | Some (Content c) -> Some c | _ -> None)))
let createWorld start =
    let grid = createGrid start
    let turtles = [
        for m, row in grid |> Array.mapi tup2 do
            for n, cell in row |> Array.mapi tup2 do
                match cell with
                | Some (Turtle id) ->
                    id, { coords = m,n; facing = North; id = id; status = Active }
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
            | Some (Victory _) -> s.Append "$"
            | None -> s.Append "."
            |> ignore
        s.Append "\n" |> ignore
    for t in world.turtles.Values do
        let dir = match t.facing with North -> "^" | South -> "v" | East -> ">" | West -> "<"
        s.AppendLine $"Turtle {t.id}: {dir}" |> ignore
    s.ToString()
let execute (turtleId:TurtleId, instruction) (world: World) : InstructionAddress option * World =
    match instruction with
    | Forward | Jump ->
        let turtle = world.turtles[turtleId]
        let m,n = turtle.coords
        let m',n' =
            match turtle.facing with
            | North -> m-1, n
            | South -> m+1, n
            | East -> m, n+1
            | West -> m, n-1
        let move() =
            None, { world with
                        turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with coords = m', n' } | _ -> None)
                        grid = world.grid |> Array2.replace m n None |> Array2.replace m' n' (Some (Turtle turtleId))
                        }
        match world.grid[m'][n'] with
        | None -> move()
        | Some(Pit) when instruction = Jump -> move()
        | Some (Wall | Pit) ->
            // kill turtle
            None, { world with
                        turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with coords = m', n'; status = Dead } | _ -> None)
                        grid = world.grid |> Array2.replace m n None |> Array2.replace m' n' (Some (Turtle turtleId)) // dead turtle body knocks down wall or fills in pit
                        }
        | Some (Turtle otherId) ->
            // kill both turtles but don't move--there's no room
            None, { world with
                        turtles = world.turtles
                                    |> Map.change turtleId (function Some t -> Some { t with status = Dead } | _ -> None)
                                    |> Map.change otherId (function Some t -> Some { t with status = Dead } | _ -> None)
                        }
        | Some Victory ->
            // turtle exits the map victorious
            None, { world with
                        turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with coords = m', n'; status = Victor } | _ -> None)
                        grid = world.grid |> Array2.replace m n None
                        }
    | Turn d ->
        let turtle = world.turtles[turtleId]
        let facing' =
            let turn = match d with | (Left | Right) as f -> f | Random -> if rand.Next 2 = 0 then Left else Right
            match turtle.facing, turn with
            | North, Left | South, Right -> West
            | West, Left | East, Right -> South
            | South, Left | North, Right -> East
            | East, Left | West, Right -> North
            | _ -> shouldntHappen()
        None, { world with turtles = world.turtles |> Map.change turtleId (function Some t -> Some { t with facing = facing' } | None -> None) }


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
                | '$' -> Victory [] |> Content |> Some
                | Number n -> StartPosition n |> Some
                | _ -> None
            |]
        )

// .fsx support for Go To Definition is worse, so putting this test logic in the code for now to ease development.
let interactive() =
    let scriptExecute n cmds world =
        // in this simple scenario we aren't concerned with instructions for goto
        cmds |> List.fold (fun world instruction -> execute(n, instruction) world |> snd) world

    let print = prettysPrint >> printfn "%s"
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
    |> scriptExecute 1 [Forward; Jump; Forward; Forward; Forward; Turn Right; Shoot; Jump; Forward; Turn Left; Forward] |> scriptExecute 2 [Forward; Turn Left; Forward]
    |> print

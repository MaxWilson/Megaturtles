module Tests
open Expecto
open Expecto.Flip
open Megaturtles.Execution
open Hopac
open Expecto.Logging
open Expecto.Logging.Message

let nonSquare = """
###
      #
  $
"""
[<Tests>]
let tests =
    testList "textToStart" [
        test "should result in a square grid" {
            let grid = textToStart nonSquare
            Expect.equal "Grid should be 3x7" (3,7) (grid.Length, (grid |> Array.map Array.length |> Array.min))
            Expect.isTrue "Grid should be 3x7" (grid |> Array.every (fun row -> row.Length = 7))
        }
    ]

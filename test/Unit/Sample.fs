module Tests
open Expecto
open Expecto.Flip
open Megaturtles.Execution

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
            Expect.equal "Grid should be 3x5" (3,5) (grid.Length, (grid |> Array.map Array.length |> Array.min))
        }
    ]

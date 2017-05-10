// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open Battleship.Models

[<EntryPoint>]
let main argv = 
    let p1 = new Player("Joe")
    let p2 = new Player("Frank")
    let game = new Game(p1, p2)

    game.placeShips p1
    game.placeShips p2
    game.printBoards p1
    game.printBoards p2

    game.playToEnd

    game.printBoards p1
    game.printBoards p2

    Console.ReadLine() |> ignore
    0 // return an integer exit code

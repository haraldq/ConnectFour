namespace Program

open Xunit
open Library

module CreateBoard = 

    [<Fact>]
    let ``Creates a board with 7 columns and 6 rows`` () = 
        let board = init 
        Assert.Equal(7, cols board)
        Assert.Equal(6, rows board)
        
module AddStone =   
        
    [<Fact>]
    let ``Adds a stone to board in given column`` () = 
        let board = init 

        addStone board Player.White 0
        
        Assert.Equal(Player.White, getPieceAt board (0,5) |> Option.get)

    [<Fact>]
    let ``Adds additional stone to board in same column`` () = 
        let board = init 
        let coord = (0,0)

        addStone board Player.White 0
        addStone board Player.White 0
        
        Assert.Equal(Player.White, getPieceAt board (0,4) |> Option.get)
        Assert.Equal(Player.White, getPieceAt board (0,5) |> Option.get)

    [<Fact>]
    let ``Adds stones to board in same column within boundaries`` () = 
        let board = init 
        let coord = (0,0)

        for i in 0.. (rows board) + 1 do
            addStone board Player.White 0
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
        let board = init |> addStone Player.White 0
        
        Assert.Equal(Player.White, getPieceAt board (0,5) |> Option.get)

    [<Fact>]
    let ``Adds additional stone to board in same column`` () = 
        let board = init |> addStone Player.White 0 |> addStone Player.White 0
        
        Assert.Equal(Player.White, getPieceAt board (0,4) |> Option.get)
        Assert.Equal(Player.White, getPieceAt board (0,5) |> Option.get)
        Assert.Equal(None, getPieceAt board (1,0))

    [<Fact>]
    let ``Adds stones to board in same column within boundaries`` () = 
        init |> 
        addStone Player.White 0 |> 
        addStone Player.White 0 |>
        addStone Player.White 0 |>
        addStone Player.White 0 |>
        addStone Player.White 0 |>
        addStone Player.White 0 |>
        addStone Player.White 0 |> ignore
        
module Winner =
    
    [<Fact>]
    let ``New board has no winner`` () =
        Assert.False(winner init Player.White)
        Assert.False(winner init Player.Black)

    [<Fact>]
    let ``4 non-adjacent in same row is not a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.White 0 |>
                    addStone Player.Black 0 |>
                    addStone Player.White 0 |>
                    addStone Player.White 0 

        Assert.False(winner board Player.White)

    [<Fact>]
    let ``4 in a row is a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.White 0 |>
                    addStone Player.White 0 |>
                    addStone Player.White 0 

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row in any column is a win`` () =
        let board = init |> 
                    addStone Player.White 2 |> 
                    addStone Player.White 2 |>
                    addStone Player.White 2 |>
                    addStone Player.White 2 

        Assert.True(winner board Player.White)

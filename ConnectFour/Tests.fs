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
    let ``4 non-adjacent vertically in same row is not a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.White 0 |>
                    addStone Player.Black 0 |>
                    addStone Player.White 0 |>
                    addStone Player.White 0 

        Assert.False(winner board Player.White)

    [<Fact>]
    let ``4 in a row vertically is a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.White 0 |>
                    addStone Player.White 0 |>
                    addStone Player.White 0 

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row vertically in any column is a win`` () =
        let board = init |> 
                    addStone Player.White 2 |> 
                    addStone Player.White 2 |>
                    addStone Player.White 2 |>
                    addStone Player.White 2 

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row horizontally is a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.White 1 |>
                    addStone Player.White 2 |>
                    addStone Player.White 3 

        Assert.True(winner board Player.White)
    
    [<Fact>]
    let ``4 non-adjacent horizontally in same row is not a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.White 1 |>
                    addStone Player.Black 2 |>
                    addStone Player.White 3 |>
                    addStone Player.White 4 

        Assert.False(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 0 row 5 down up is a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.Black 1 |>
                    addStone Player.White 1 |>
                    addStone Player.Black 2 |>
                    addStone Player.Black 2 |>
                    addStone Player.White 2 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |> 
                    addStone Player.White 3

        Assert.True(winner board Player.White)

        (*
            
            
        *)


    [<Fact>]
    let ``4 in a row non-adjacent diagonally from column 0 down up is not a win`` () =
        let board = init |> 
                    addStone Player.White 0 |> 
                    addStone Player.Black 1 |>
                    addStone Player.White 1 |>
                    addStone Player.Black 2 |>
                    addStone Player.Black 2 |>
                    addStone Player.White 2 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |> 
                    addStone Player.Black 3 |> 
                    addStone Player.Black 4 |>
                    addStone Player.Black 4 |>
                    addStone Player.Black 4 |>
                    addStone Player.Black 4 |>
                    addStone Player.White 4

        Assert.False(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 1 row 5 down up is a win`` () =
        let board = init |>  
                    addStone Player.White 1 |>
                    addStone Player.Black 2 |>
                    addStone Player.White 2 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |> 
                    addStone Player.White 3 |> 
                    addStone Player.Black 4 |>
                    addStone Player.Black 4 |>
                    addStone Player.Black 4 |>
                    addStone Player.White 4

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 2 row 5 down up is a win`` () =
        let board = init |>  
                    addStone Player.White 2 |>
                    addStone Player.Black 3 |>
                    addStone Player.White 3 |> 
                    addStone Player.Black 4 |>
                    addStone Player.Black 4 |>
                    addStone Player.White 4 |>
                    addStone Player.Black 5 |>
                    addStone Player.Black 5 |>
                    addStone Player.Black 5 |>
                    addStone Player.White 5

        Assert.True(winner board Player.White)
    
    [<Fact>]
    let ``4 in a row diagonally from column 3 row 5 down up is a win`` () =
        let board = init |>  
                    addStone Player.White 3 |>
                    addStone Player.Black 4 |>
                    addStone Player.White 4 |>
                    addStone Player.Black 5 |>
                    addStone Player.Black 5 |>
                    addStone Player.White 5 |>
                    addStone Player.Black 6 |>
                    addStone Player.Black 6 |>
                    addStone Player.Black 6 |>
                    addStone Player.White 6

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 0 row 4 down up is a win`` () =
        let board = init |>  
                    addStone Player.Black 0 |>
                    addStone Player.White 0 |> 
                    addStone Player.Black 1 |>
                    addStone Player.Black 1 |>
                    addStone Player.White 1 |>
                    addStone Player.Black 2 |>
                    addStone Player.Black 2 |>
                    addStone Player.Black 2 |>
                    addStone Player.White 2 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |> 
                    addStone Player.Black 3 |> 
                    addStone Player.White 3

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 0 row 3 down up is a win`` () =
        let board = init |>  
                    addStone Player.Black 0 |>
                    addStone Player.Black 0 |>
                    addStone Player.White 0 |> 
                    addStone Player.Black 1 |>
                    addStone Player.Black 1 |>
                    addStone Player.Black 1 |>
                    addStone Player.White 1 |>
                    addStone Player.Black 2 |>
                    addStone Player.Black 2 |>
                    addStone Player.Black 2 |>
                    addStone Player.Black 2 |>
                    addStone Player.White 2 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |>
                    addStone Player.Black 3 |> 
                    addStone Player.Black 3 |> 
                    addStone Player.White 3

        Assert.True(winner board Player.White)
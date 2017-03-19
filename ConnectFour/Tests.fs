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

    let addNumberOfStonesOnRow (n: int) (row:int) (board: Board)= 
        let rec asut board n = 
            match n with 
            | 0 -> board
            | 1 -> addStone Player.White row board
            | _ -> asut (addStone Player.Black row board) (n - 1)
        asut board n
        
    [<Fact>]
    let ``4 in a row diagonally from column 0 row 5 down up is a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 1 0 |>
                     addNumberOfStonesOnRow 2 1 |>
                     addNumberOfStonesOnRow 3 2 |>
                     addNumberOfStonesOnRow 4 3

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row non-adjacent diagonally from column 0 down up is not a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 1 0 |>
                     addNumberOfStonesOnRow 2 1 |>
                     addNumberOfStonesOnRow 4 2 |>
                     addNumberOfStonesOnRow 4 3 |>
                     addNumberOfStonesOnRow 5 4

        Assert.False(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 1 row 5 down up is a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 1 1 |>
                     addNumberOfStonesOnRow 2 2 |>
                     addNumberOfStonesOnRow 3 3 |>
                     addNumberOfStonesOnRow 4 4

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 2 row 5 down up is a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 1 2 |>
                     addNumberOfStonesOnRow 2 3 |>
                     addNumberOfStonesOnRow 3 4 |>
                     addNumberOfStonesOnRow 4 5

        Assert.True(winner board Player.White)
    
    [<Fact>]
    let ``4 in a row diagonally from column 3 row 5 down up is a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 1 3 |>
                     addNumberOfStonesOnRow 2 4 |>
                     addNumberOfStonesOnRow 3 5 |>
                     addNumberOfStonesOnRow 4 6

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 0 row 4 down up is a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 2 1 |>
                     addNumberOfStonesOnRow 3 2 |>
                     addNumberOfStonesOnRow 4 3 |>
                     addNumberOfStonesOnRow 5 4

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 0 row 3 down up is a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 3 0 |>
                     addNumberOfStonesOnRow 4 1 |>
                     addNumberOfStonesOnRow 5 2 |>
                     addNumberOfStonesOnRow 6 3

        Assert.True(winner board Player.White)

    [<Fact>]
    let ``4 in a row diagonally from column 0 row 0 up down is a win`` () =
        let board = init |>
                     addNumberOfStonesOnRow 6 0 |>
                     addNumberOfStonesOnRow 5 1 |>        
                     addNumberOfStonesOnRow 4 2 |>
                     addNumberOfStonesOnRow 3 3
        Assert.True(winner board Player.White)
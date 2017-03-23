namespace Program

module Library = 

    type Column = int
    type Row = int
    type Coordinate = Column * Row
 
    type Player =
        | White
        | Black
 
    type BoardCell = Player option
 
    type BoardColumn = BoardCell array
 
    type Board = BoardColumn array

    let init : Board =
        Array.create 7 (Array.create 6 None)

    let cols (board: Board) : int = 
        Array.length board

    let rows (board: Board) : int = 
        Array.length (board.[0])
    
    let getPieceAt (board: Board) ((c,r): Coordinate): BoardCell =
        board.[c].[r]

    let newBoardWithStone (board: Board)  ((c,r): Coordinate) (player: Player): Board = 
        Array.init (cols board) (fun c' -> 
            Array.init (rows board) (fun r' -> if (c', r') = (c, r) then Some(player) else getPieceAt board (c', r')))

    let addStone (player: Player) col  (board: Board) = 
        let rec addStone' (board: Board) (player: Player) col acc =
            match getPieceAt board (col, acc)  with
            | Some(value) when acc = 0 -> board
            | Some(value) -> addStone' board player col (acc - 1) 
            | None -> newBoardWithStone board (col, acc) player
        addStone' board player col (rows board - 1)
        
    let checkVertical (board: Board) (player: Player) col =
        [0..rows board - 1] 
             |> List.map (fun r-> getPieceAt board (col, r))
             |> List.windowed 4
             |> List.exists (fun x -> x |> List.forall(fun y -> y = Some(player)))

    let checkLine (board: Board) (player: Player) (f : int -> Coordinate) =
        [0..rows board - 1] 
             |> List.map (fun x-> getPieceAt board (f x))
             |> List.windowed 4
             |> List.exists (fun x -> x |> List.forall(fun y -> y = Some(player)))

    let checkDiag (board: Board) (player: Player) (start: Coordinate) fCoordTraversal = 
        let (startCol, startRow) = start
        let inBoard ((col, row): Coordinate) =  0 <= col && col < cols board && 0 <= row && row < rows board

        let coords = [0..5] |> List.map (fun x -> fCoordTraversal startCol startRow x) |> List.filter (fun x -> inBoard x)

        let pieces = coords |> List.map (fun x -> getPieceAt board x)

        pieces
        |> List.windowed 4
        |> List.exists (fun x -> x |> List.forall(fun y -> y = Some(player)))

    let winner (board: Board) (player: Player) =
        let fCol x y = (x,y)
        let checkVerticals = [0..cols board - 1] 
                                |> List.exists (fun x -> checkLine board player (fCol x))
        let fRow x y = (y,x)   
        let checkHorizontals = [0..rows board - 1]
                                |> List.exists (fun x -> checkLine board player (fRow x))
        
        let fCoordsTraversalUD startCol startRow x = (startCol + x, startRow - x)
        let checkDiagonal = [0..5] |> List.map(fun x -> checkDiag board player (x,5) fCoordsTraversalUD) 
                                   |> List.exists(fun x -> x = true)
        let checkDiagonal' = [0..5] |> List.map(fun x -> checkDiag board player (0,x) fCoordsTraversalUD) 
                                   |> List.exists(fun x -> x = true)
                                   
        let fCoordsTraversalDU startCol startRow x = (startCol + x, startRow + x)
        let checkDiagonal'' = [0..5] |> List.map(fun x -> checkDiag board player (x,0) fCoordsTraversalDU) 
                                   |> List.exists(fun x -> x = true)
        let checkDiagonal''' = [0..5] |> List.map(fun x -> checkDiag board player (5,x) fCoordsTraversalDU) 
                                   |> List.exists(fun x -> x = true)
        

        checkVerticals || checkHorizontals ||  checkDiagonal || checkDiagonal' || checkDiagonal'' || checkDiagonal'''
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

    let checkDiagDownUp (board: Board) (player: Player) (start: Coordinate) = 
        let (startCol, startRow) = start
        let inBoard ((col, row): Coordinate) =  0 <= col && col < cols board && 0 <= row && row < rows board

        let coords = [0..5] |> List.map (fun x -> (startCol + x, startRow - x)) |> List.filter (fun x -> inBoard x)

        let pieces = coords |> List.map (fun x -> getPieceAt board x)

        pieces
        |> List.windowed 4
        |> List.exists (fun x -> x |> List.forall(fun y -> y = Some(player)))

    let checkDiagUpDown (board: Board) (player: Player) (start: Coordinate) = 
        let (startCol, startRow) = start
        let inBoard ((col, row): Coordinate) =  0 <= col && col < cols board && 0 <= row && row < rows board

        let coords = [0..5] |> List.map (fun x -> (startCol + x, startRow + x)) |> List.filter (fun x -> inBoard x)

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
                                
        let checkDiagonalUD =  checkDiagDownUp board player (0,5) 
        let checkDiagonalUD' =  checkDiagDownUp board player (1,5) 
        let checkDiagonalUD'' =  checkDiagDownUp board player (2,5)  
        let checkDiagonalUD''' =  checkDiagDownUp board player (3,5)  
        let checkDiagonalUD'''' =  checkDiagDownUp board player (0,4)  
        let checkDiagonalUD''''' =  checkDiagDownUp board player (0,3)  
        let checkDiagonalDU =  checkDiagUpDown board player (0,0)  
        let checkDiagonalDU' =  checkDiagUpDown board player (1,0)  
        

        checkVerticals || checkHorizontals || checkDiagonalUD || checkDiagonalUD' || checkDiagonalUD'' || checkDiagonalUD'''
        || checkDiagonalUD'''' || checkDiagonalUD''''' || checkDiagonalDU || checkDiagonalDU'
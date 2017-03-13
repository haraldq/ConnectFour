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
            | None -> newBoardWithStone board (col, acc) player //board.[col].[acc] <- Some(player)
        addStone' board player col (rows board - 1)
        

    let winner (board: Board): Player option =
        let verticalLine = [0..cols board - 1] |> List.map (fun c-> getPieceAt board (c, 0))
        None
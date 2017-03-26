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
        
    let checkLine (board: Board) (player: Player) (start: Coordinate) fCoordTraversal = 
        let (startCol, startRow) = start
        let inBoard ((col, row): Coordinate) =  0 <= col && col < cols board && 0 <= row && row < rows board

        [0..cols board - 1] |> 
            List.map (fun x -> fCoordTraversal startCol startRow x) |> 
            List.filter (fun x -> inBoard x) |>
            List.map (fun x -> getPieceAt board x)
            |> List.windowed 4

    let winner (board: Board) (player: Player) =
        let nCols = cols board - 1
        let nRows = rows board - 1

        let vertTraverse c r x = (c, r + x)
        let verticals = [0..nCols] |> List.map(fun x -> checkLine board player (x,0) vertTraverse) 

        let horTraverse c r x = (c + x, r)
        let horizontals = [0..nCols] |> List.map(fun x -> checkLine board player (0,x) horTraverse) 

        let updownDiagTraverse c r x = (c + x, r - x)
        let diag = [0..nCols] |> List.map(fun x -> checkLine board player (x,nCols) updownDiagTraverse) 
        let diag' = [0..nCols] |> List.map(fun x -> checkLine board player (0,x) updownDiagTraverse) 
                                   
        let downupDiagTraverse c r x = (c + x, r + x)
        let diag'' = [0..nCols] |> List.map(fun x -> checkLine board player (x,0) downupDiagTraverse) 
        let diag''' = [0..nCols] |> List.map(fun x -> checkLine board player (nCols,x) downupDiagTraverse) 

        let all = List.concat [verticals;horizontals;diag;diag';diag'';diag''']

        all |> List.exists(fun x -> x |> List.exists (fun y -> y |> List.forall(fun z -> z = Some(player))))
        
module ConnectFour.Model

(******************************************************************************)

type Config = {
    Rows : int
    Columns : int
    Win : int
    Depth : int
}

let defaultConfig = {
    Rows = 6
    Columns = 7
    Win = 5
    Depth = 6
}

module Config =
    
    let ofRowsColumns rows columns =
        { defaultConfig with
            Rows = rows
            Columns = columns }
        
    let withWin win cfg =
        { cfg with Win = win }

(******************************************************************************)

let config : Reader<Config, Config> = Reader id

type Player =
    | O
    | B
    | X
    
type Row = List<Player>

type Column = List<Player>

type Board = List<Row>

let rows board = board

let columns (board : Board) = List.transpose board

/// First /, then \
let diagonals (board : Board) = reader {
    let shiftedRows (shift : int) (rowMultiplier : int) =
        let result = 
            board
            |> List.mapi(fun rowIndex row ->
                let t =
                    row
                    |> List.safeSkip (shift + rowIndex * rowMultiplier)
                    |> List.truncate 1
                t)
            |> List.filter (not << List.isEmpty)
            |> List.transpose
        result

    let! cfg = config
    return 
        [
            for shift in [0 .. cfg.Columns + cfg.Rows - 2] do
                shiftedRows shift -1
            for shift in [-(cfg.Rows - 1) .. cfg.Columns - 1] do
                shiftedRows shift 1
        ]
        |> List.filter (not << List.isEmpty)
        |> List.concat
}
    
let winnerInRow (row : Row) = reader {
    let! cfg = config
    let toWin = cfg.Win

    let rec impl (prev, count) cells =
        match cells with
        | [] -> None
        | B :: rest -> impl (B, 1) rest
        | player :: _ when player = prev && count + 1 >= toWin -> Some player
        | player :: rest when player = prev -> impl (player, count + 1) rest
        | player :: rest -> impl (player, 1) rest
    
    return impl (B, 0) row
}

let winner (board : Board) = reader {
    let! diagonals = diagonals board
    let! winners =
        rows board @ columns board @ diagonals
        |> List.map winnerInRow
        |> Reader.join
        
    return winners
        |> List.choose id
        |> List.tryHead
}

let isFullColumn (column : Column) =
    column.Head <> B

let tryAdd (player : Player) (column : Column) =
    let go current (added, soFar) =
        match current, added with
        | _, true -> (true, current :: soFar)
        | B, _ -> (true, player :: soFar)
        | current, _ -> (false, current :: soFar)
            
    let added, result = List.foldBack go column (false, [])
    if added then
        Some result
    else
        None

let nextMoves (player : Player) (board : Board) : List<Board> =
    board
    |> columns
    |> Zipper.fromList
    |> Zipper.selfAndRights
    |> List.choose (fun z -> tryAdd player z.Focus |> Option.map (fun column -> Zipper.withFocus column z))
    |> List.map (Zipper.toList >> columns)
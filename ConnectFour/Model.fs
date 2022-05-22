module ConnectFour.Model

open System
open System.Diagnostics

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

module Player =
    
    let displayString = function
        | O -> "o"
        | B -> " "
        | X -> "x"
    
let nextPlayer = function
    | O -> X
    | B -> failwith "Blank cell is not a player"
    | X -> O
    
type Row = List<Player>

type Column = List<Player>

[<Struct>]
[<DebuggerDisplay("{ToString(),nq}")>]
type Board = Board of List<Row> with
    override this.ToString () =
        let (Board rows) = this
        let rowsStrings =
            rows
            |> List.map (fun row -> String.Join("", row |> List.map Player.displayString))
        String.Join(Environment.NewLine, rowsStrings)

let rows (Board board) = board

let columns (Board board) = List.transpose board

/// First /, then \
let diagonals (Board board) = reader {
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
    |> List.map (Zipper.toList >> Board >> columns >> Board)
    
type GameTreeNode = {
    PlayerToPlay : Player
    Board : Board
    Depth : int
}
    
let buildGameTree (playerToPlay : Player) (board : Board) = reader {
    let! cfg = config
    let rec impl currentDepth (playerToPlay : Player) (board : Board) =
        if currentDepth >= cfg.Depth then
            let tree = {
                Value = {
                    PlayerToPlay = playerToPlay
                    Depth = currentDepth
                    Board = board
                }
                Children = []
            }
            tree
        else
            let nextMoves = nextMoves playerToPlay board
            let tree = {
                Value = {
                    PlayerToPlay = playerToPlay
                    Depth = currentDepth
                    Board = board
                }
                Children =
                    nextMoves
                    |> List.map (impl (currentDepth + 1) (nextPlayer playerToPlay))
            }
            tree
    return impl 0 playerToPlay board
}
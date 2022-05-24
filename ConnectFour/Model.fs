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
    Rows = 3
    Columns = 3
    Win = 3
    Depth = 6
}

module Config =
    
    let ofRowsColumns rows columns =
        { defaultConfig with
            Rows = rows
            Columns = columns }
        
    let withWin win cfg =
        { cfg with Win = win }
        
    let withDepth depth cfg =
        { cfg with Depth = depth }

(******************************************************************************)

let config : Reader<Config, Config> = Reader id

type Player =
    | O
    | B
    | X

module Player =
    
    let displayString = function
        | O -> "o"
        | B -> "."
        | X -> "x"
        
    let minimax (player : Player) (childrenWinners : List<Player>) =
        match player, childrenWinners with
        | B, _ -> B
        | _, [] -> B
        | O, winners -> List.min winners
        | X, winners -> List.max winners
    
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
        
let mkBoard (cfg : Config) =
    List.replicate cfg.Columns B
    |> List.replicate cfg.Rows
    |> Board

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
    
let isFullBoard (board : Board) =
    board
    |> columns
    |> List.forall isFullColumn

let tryAddToColumn (player : Player) (column : Column) : Option<Column> =
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
        
let tryAddToBoard (player : Player) (columnIndex : int) (board : Board) : Option<Board> =
    let boardColumns = board |> columns
    let rec go currentColumnIndex soFar columns =
        match currentColumnIndex, columns with
        | _, [] ->
            // No more columns
            None
        | 0, column :: rest ->
            let column' = tryAddToColumn player column
            match column' with
            | None -> None
            | Some column' -> Some ^ (List.rev soFar) @ [column'] @ rest
        | currentColumnIndex, column :: rest ->
            go (currentColumnIndex - 1) (column :: soFar) rest
    
    let columns' = go columnIndex [] boardColumns
    columns' |> Option.map (List.transpose >> Board)

let nextMoves (player : Player) (board : Board) : List<Board> =
    board
    |> columns
    |> Zipper.fromList
    |> Zipper.selfAndRights
    |> List.choose (fun z -> tryAddToColumn player z.Focus |> Option.map (fun column -> Zipper.withFocus column z))
    |> List.map (Zipper.toList >> Board >> columns >> Board)
    
type GameTreeNode = {
    PlayerToPlay : Player
    Winner : Player
    Board : Board
    Depth : int
}
    
let buildGameTree (playerToPlay : Player) (board : Board) = reader {
    let! cfg = config
    let rec impl currentDepth (playerToPlay : Player) (board : Board) = reader {
        // todo notify on depth exhaustion
        if currentDepth >= cfg.Depth then
            let tree = {
                Value = {
                    PlayerToPlay = playerToPlay
                    Winner = B
                    Depth = currentDepth
                    Board = board
                }
                Children = []
            }
            return tree
        else
            let nextPlayer = nextPlayer playerToPlay
            let! winner = winner board
            let! winner, children =
                match winner with
                | Some w -> reader.Return (w, [])
                | None -> reader {
                    let! children =
                        nextMoves nextPlayer board
                        |> List.map (impl (currentDepth + 1) nextPlayer)
                        |> Reader.join
                    let childrenWinners =
                        children
                        |> List.map (fun child -> child.Value.Winner)
                    let nodeWinner = Player.minimax playerToPlay childrenWinners
                    return (nodeWinner, children)
                }
            let tree = {
                Value = {
                    PlayerToPlay = playerToPlay
                    Winner = winner
                    Depth = currentDepth
                    Board = board
                }
                Children = children
            }
            return tree
        }
    return! impl 0 playerToPlay board
}

type AIMove =
    | Definite of Board
    | RandomGuess of Board

let nextMove (currentPlayer : Player) (board : Board) = reader {
    let! tree = buildGameTree (nextPlayer currentPlayer) board
    let randomMove (choices : List<Board>) =
        match choices with
        | [] ->
            // No more choices
            None
        | choices ->
            let randomIndex = Random.Shared.Next(choices.Length)
            List.item randomIndex choices |> RandomGuess |> Some

    let nextMove =
        tree.Children
        |> List.filter (fun child -> child.Value.Winner = currentPlayer)
        |> List.tryHead
        |> Option.map (fun child -> Definite child.Value.Board)
        |> Option.orElseWith (fun () ->
            tree.Children
            |> List.filter (fun child -> child.Value.Winner = B)
            |> List.map (fun child -> child.Value.Board)
            |> randomMove)
        |> Option.orElseWith (fun () ->
            tree.Children
            |> List.map (fun child -> child.Value.Board)
            |> randomMove)
    return nextMove
}

(******************************************************************************)

type Outcome =
    | Win of Player
    | Draw
    | InProgress
    
let boardOutcome (board : Board) = reader {
    let! winner = winner board
    match winner with
    | Some w -> return Win w
    | None ->
        if isFullBoard board then
            return Draw
        else
            return InProgress
}
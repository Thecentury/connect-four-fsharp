module ConnectFour.Model

// todo mikbri use reader monad to provide these constants?

let rowsNum = 6
let colsNum = 7
let win = 5
let depth = 6

type Player =
    | O
    | Blank
    | X
    
type Row = List<Player>

type Board = List<Row>

let rows board = board

let columns (board : Board) = List.transpose board

let diagonals (board : Board) =
    let shiftedRows (shift : int) (rowMultiplier : int) =
        board
        |> List.mapi(fun rowIndex row ->
            row |> List.safeSkip (shift + rowIndex * rowMultiplier))
        |> List.filter (not << List.isEmpty)

    [
        for multiplier in [1; -1] do
            for shift in [0 .. colsNum + rowsNum - 1] do
                shiftedRows shift multiplier |> List.transpose
    ]
    |> List.concat
    
let winnerInRow (row : Row) =
    let rec impl (prev, count) cells =
        match cells with
        | [] -> None
        | Blank :: rest -> impl (Blank, 1) rest
        | player :: _ when player = prev && count + 1 >= win -> Some player
        | player :: rest when player = prev -> impl (player, count + 1) rest
        | player :: rest -> impl (player, 1) rest
    
    impl (Blank, 0) row

let winner (board : Board) =
    rows board @ columns board @ diagonals board
    |> List.choose winnerInRow
    |> List.tryHead

let nextMoves (player : Player) (board : Board) = []

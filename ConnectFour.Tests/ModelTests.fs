module ConnectFour.Tests.ModelTests

open ConnectFour
open ConnectFour.Model
open Xunit
open Swensen.Unquote

let board : Board = Board [
    [B; B; B; B; B; B; B]
    [B; B; B; B; B; B; B]
    [B; B; B; B; B; B; B]
    [B; B; B; B; B; B; B]
    [B; B; B; O; B; B; B]
    [B; B; B; X; O; B; B]
]

[<Fact>]
let ``Calculates diagonals of a 1x1 board`` () =
    let board = Board [[X]]
    let cfg = Config.ofRowsColumns 1 1
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 2x1 board`` () =
    let board = Board [
        [X]
        [O]
    ]
    let cfg = Config.ofRowsColumns 2 1
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [O]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 1x2 board`` () =
    let board = Board [[X; O]]
    let cfg = Config.ofRowsColumns 1 2
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [X]; [O]] @>

[<Fact>]
let ``Calculates diagonals of a 1x3 board`` () =
    let board = Board [[X; O; X]]
    let cfg = Config.ofRowsColumns 1 3
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [X]; [X]; [O]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 3x1 board`` () =
    let board = Board [
        [X]
        [O]
        [X]
    ]
    let cfg = Config.ofRowsColumns 3 1
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [X]; [X]; [O]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 2x2 board`` () =
    let board = Board [
        [X; B]
        [O; B]
    ]
    let cfg = Config.ofRowsColumns 2 2
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [
        [X]
        [B; O]
        [B]
        [O]
        [X; B]
        [B]
    ] @>

(******************************************************************************)

[<Fact>]
let ``No winner`` () =
    let board = Board [
        [X; B]
        [O; B]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = None @>

[<Fact>]
let ``Winner in a row`` () =
    let board = Board [
        [X; X]
        [O; B]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = Some X @>
    
[<Fact>]
let ``Winner in a column`` () =
    let board = Board [
        [X; B]
        [X; O]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = Some X @>

[<Fact>]
let ``Winner in a diagonal`` () =
    let board = Board [
        [X; B]
        [O; X]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = Some X @>
    
(******************************************************************************)

[<Fact>]
let ``TryAdd to a full column`` () =
    let column' = tryAddToColumn O [X; O]
    test <@ column' = None @>
    
[<Fact>]
let ``TryAdd to an not empty column`` () =
    let column' = tryAddToColumn X [B; O]
    test <@ column' = Some [X; O] @>
    
(******************************************************************************)

[<Fact>]
let ``nextMoves of an empty 1x1 board`` () =
    let board = Board [[B]]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [Board [[O]]] @>
    
[<Fact>]
let ``nextMoves of an empty 2x1 board`` () =
    let board = Board [[B; B]]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        Board [[O; B]]
        Board [[B; O]]
    ] @>

[<Fact>]
let ``nextMoves of an empty 2x2 board`` () =
    let board = Board [
        [B; B]
        [B; B]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        Board [
            [B; B]
            [O; B]
        ]
        Board [
            [B; B]
            [B; O]
        ]
    ] @>

[<Fact>]
let ``nextMoves of a non-empty 2x2 board`` () =
    let board = Board [
        [B; B]
        [X; B]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        Board [
            [O; B]
            [X; B]
        ]
        Board [
            [B; B]
            [X; O]
        ]
    ] @>

[<Fact>]
let ``nextMoves of an 2x2 board with one full column`` () =
    let board = Board [
        [O; B]
        [X; B]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        Board [
            [O; B]
            [X; O]
        ]
    ] @>

[<Fact>]
let ``nextMoves of a full 2x2 board`` () =
    let board = Board [
        [O; X]
        [X; O]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [] @>

(******************************************************************************)

[<Fact>]
let ``buildGameTree for a board from video`` () =
    let board = Board [
        [B; B; X]
        [B; X; O]
        [O; O; X]
    ]
    let cfg = Config.ofRowsColumns 3 3 |> Config.withWin 3
    let gameTree = Reader.run cfg ^ buildGameTree Player.O board
    let _gameTreeAsString = $"%A{gameTree}"
    test <@ gameTree.Value.Winner = WinnerInChildren X @>
    
[<Fact>]
let ``buildGameTree for after the first move`` () =
    let board = Board [
        [B; B; B]
        [B; B; B]
        [O; B; B]
    ]
    let cfg = Config.ofRowsColumns 3 3 |> Config.withWin 3 |> Config.withDepth 7
    let gameTree = Reader.run cfg ^ buildGameTree Player.O board
    let _gameTreeAsString = $"%A{gameTree}"
    ()
    
(******************************************************************************)

[<Fact>]
let ``nextMove when only one move remains`` () =
    let board = Board [
        [X; X; B]
        [O; O; X]
        [O; X; O]
    ]
    let cfg = Config.ofRowsColumns 3 3 |> Config.withWin 3 |> Config.withDepth 7
    let nextMove = Reader.run cfg ^ nextMove Player.O board
    match nextMove with
    | Some (Definite board)
    | Some (RandomGuess board) -> 
        test <@ board = Board [
            [X; X; O]
            [O; O; X]
            [O; X; O]
        ] @>
    | None -> failwith "nextMove should not be None"
module ConnectFour.Tests.ModelTests

open ConnectFour
open ConnectFour.Model
open Xunit
open Swensen.Unquote

let board : Board = [
    [B; B; B; B; B; B; B]
    [B; B; B; B; B; B; B]
    [B; B; B; B; B; B; B]
    [B; B; B; B; B; B; B]
    [B; B; B; O; B; B; B]
    [B; B; B; X; O; B; B]
]

[<Fact>]
let ``Calculates diagonals of a 1x1 board`` () =
    let board = [[X]]
    let cfg = Config.ofRowsColumns 1 1
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 2x1 board`` () =
    let board = [
        [X]
        [O]
    ]
    let cfg = Config.ofRowsColumns 2 1
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [O]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 1x2 board`` () =
    let board = [[X; O]]
    let cfg = Config.ofRowsColumns 1 2
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [X]; [O]] @>

[<Fact>]
let ``Calculates diagonals of a 1x3 board`` () =
    let board = [[X; O; X]]
    let cfg = Config.ofRowsColumns 1 3
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [X]; [X]; [O]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 3x1 board`` () =
    let board = [
        [X]
        [O]
        [X]
    ]
    let cfg = Config.ofRowsColumns 3 1
    let diagonals = Reader.run cfg (diagonals board)
    
    test <@ diagonals = [[X]; [O]; [X]; [X]; [O]; [X]] @>

[<Fact>]
let ``Calculates diagonals of a 2x2 board`` () =
    let board = [
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
    let board = [
        [X; B]
        [O; B]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = None @>

[<Fact>]
let ``Winner in a row`` () =
    let board = [
        [X; X]
        [O; B]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = Some X @>
    
[<Fact>]
let ``Winner in a column`` () =
    let board = [
        [X; B]
        [X; O]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = Some X @>

[<Fact>]
let ``Winner in a diagonal`` () =
    let board = [
        [X; B]
        [O; X]
    ]
    let cfg = Config.ofRowsColumns 2 2 |> Config.withWin 2

    let winner = Reader.run cfg (winner board)

    test <@ winner = Some X @>
    
(******************************************************************************)

[<Fact>]
let ``TryAdd to a full column`` () =
    let column' = tryAdd O [X; O]
    test <@ column' = None @>
    
[<Fact>]
let ``TryAdd to an not empty column`` () =
    let column' = tryAdd X [B; O]
    test <@ column' = Some [X; O] @>
    
(******************************************************************************)

[<Fact>]
let ``nextMoves of an empty 1x1 board`` () =
    let board = [[B]]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [[[O]]] @>
    
[<Fact>]
let ``nextMoves of an empty 2x1 board`` () =
    let board = [[B; B]]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        [[O; B]]
        [[B; O]]
    ] @>

[<Fact>]
let ``nextMoves of an empty 2x2 board`` () =
    let board = [
        [B; B]
        [B; B]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        [
            [B; B]
            [O; B]
        ]
        [
            [B; B]
            [B; O]
        ]
    ] @>

[<Fact>]
let ``nextMoves of a non-empty 2x2 board`` () =
    let board = [
        [B; B]
        [X; B]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        [
            [O; B]
            [X; B]
        ]
        [
            [B; B]
            [X; O]
        ]
    ] @>

[<Fact>]
let ``nextMoves of an 2x2 board with one full column`` () =
    let board = [
        [O; B]
        [X; B]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [
        [
            [O; B]
            [X; O]
        ]
    ] @>

[<Fact>]
let ``nextMoves of a full 2x2 board`` () =
    let board = [
        [O; X]
        [X; O]
    ]
    let nextMoves = nextMoves Player.O board
    
    test <@ nextMoves = [] @>


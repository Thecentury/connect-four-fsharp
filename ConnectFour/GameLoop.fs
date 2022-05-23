module ConnectFour.GameLoop

open System
open ConnectFour.Model

let rec readPlayerInput (player : Player) =
    reader {
        let! cfg = config
        printf $"Player %A{player}, enter column number: "
        let column = Console.ReadLine()

        match column with
        | Int column when column >= 1 && column <= cfg.Columns ->
            // convert to zero-based index
            return column - 1
        | _ ->
            printfn "Invalid column number, try again."
            return! readPlayerInput player
    }

let (|AI|Human|) (player : Player) =
    match player with
    | X -> AI
    | O -> Human
    | B -> failwith "Invalid player B"

let drawBoard (board : Board) =
    reader {
        let! cfg = config

        rows board
        |> List.iter (fun row ->
            let rowString =
                String.Join("", row |> List.map Player.displayString)

            printfn $"%s{rowString}")

        [ 1 .. cfg.Columns ]
        |> List.iter (fun _ -> printf "-")
        printfn ""

        [ 1 .. cfg.Columns ]
        |> List.iter (fun column -> printf $"%i{column}")
        printfn ""
        printfn ""

        return ()
    }

let rec loop (player : Player) (board : Board) =
    reader {
        do! drawBoard board
        let! outcome = boardOutcome board

        match outcome with
        | Outcome.Win winner ->
            printfn $"Player %A{winner} wins!"
            return ()
        | Outcome.Draw ->
            printfn "Draw!"
            return ()
        | Outcome.InProgress ->
            match player with
            | AI ->
                let! board' = nextMove player board

                match board' with
                | Some (Definite board') ->
                    printfn "I'll win!"
                    return! loop (nextPlayer player) board'
                | Some (RandomGuess board') ->
                    printfn "I hope I'll win..."
                    return! loop (nextPlayer player) board'
                | None ->
                    printfn "AI failed to make a move."
                    return ()
            | Human ->
                let! column = readPlayerInput player

                let board' =
                    tryAddToBoard player column board

                match board' with
                | Some board' -> return! loop (nextPlayer player) board'
                | None ->
                    printfn $"Column %i{column} is full, choose another one."
                    return! loop player board
    }

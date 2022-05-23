module ConnectFour.BoardComparison

open ConnectFour.Model

type CellDiff =
    | Unchanged of Player
    | Changed of Player
    
let private cellDiff (prev : Player) (cur : Player) : CellDiff =
    if prev = cur then
        Unchanged prev
    else
        Changed cur
        
let diffBoards (Board prev) (Board cur) : List<List<CellDiff>> =
    List.zip prev cur
    |> List.map (fun (prevRow, curRow) ->
        List.zip prevRow curRow
        |> List.map (fun (prevCell, curCell) -> cellDiff prevCell curCell)
    )
open ConnectFour
open ConnectFour.Model
open ConnectFour.GameLoop

let cfg = {
    Rows = 3
    Columns = 3
    Win = 3
    Depth = 10
}

let initialBoard = mkBoard cfg

Reader.run cfg (loop Player.O initialBoard)
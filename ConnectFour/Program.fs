open ConnectFour
open ConnectFour.Model
open ConnectFour.GameLoop

let cfg = {
    Rows = 3
    Columns = 3
    Win = 3
    Depth = 10
}

let initialBoard = mkBoard defaultConfig

Reader.run defaultConfig (loop Player.O initialBoard)
open ConnectFour
open ConnectFour.Model
open ConnectFour.GameLoop

let cfg = {
    Rows = 3
    Columns = 3
    Win = 3
    Depth = 10
}

let run cfg =
    let board = mkBoard cfg
    Reader.run cfg (loop Player.O board)

run defaultConfig
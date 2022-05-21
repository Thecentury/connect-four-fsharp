[<AutoOpen>]
module ConnectFour.Prelude

module List =

    let safeSkip count list =
        if count <= 0 then
            list
        else
            let rec loop i lst =
                match lst with
                | _ when i = 0 -> lst
                | _ :: t -> loop (i - 1) t
                | [] -> []

            loop count list

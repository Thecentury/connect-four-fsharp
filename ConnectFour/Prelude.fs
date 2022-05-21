[<AutoOpen>]
module ConnectFour.Prelude

module List =

    let safeSkip count list =
        if count < 0 then
            []
        elif count = 0 then
            list
        else
            let rec loop i lst =
                match lst with
                | _ when i = 0 -> lst
                | _ :: t -> loop (i - 1) t
                | [] -> []

            loop count list

(******************************************************************************)

[<Struct>]
type Reader<'env, 'a> = Reader of action : ('env -> 'a)

module Reader =
    /// Run a Reader with a given environment
    let run env (Reader action) = action env // simply call the inner function

    /// Create a Reader which returns the environment itself
    let ask = Reader id

    /// Map a function over a Reader
    let map f reader = Reader(fun env -> f (run env reader))

    /// flatMap a function over a Reader
    let bind f reader =
        let newAction env =
            let x = run env reader
            run env (f x)

        Reader newAction
        
    let join readers =
        let newAction env =
            List.map (run env) readers

        Reader newAction

type ReaderBuilder() =
    member _.Return(x) = Reader(fun _ -> x)
    member _.Bind(x, f) = Reader.bind f x
    member _.Zero() = Reader(fun _ -> ())
    member _.ReturnFrom x = x

let reader = ReaderBuilder()

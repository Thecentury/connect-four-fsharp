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

(******************************************************************************)

type Zipper<'a> = {
    Left : List<'a>
    Focus : 'a
    Right : List<'a>
}

module Zipper =

    let private create left focus right =
        {
            Left = left
            Focus = focus
            Right = right
        }
        
    let fromList list = {
        Left = []
        Focus = List.head list
        Right = List.tail list
    }
    
    let withFocus f zipper =
        { zipper with Focus = f }
    
    let tryMoveLeft zipper =
        match zipper.Left with
        | [] -> None
        | head :: tail ->
            {
                Left = tail
                Focus = head
                Right = zipper.Focus :: zipper.Right
            } |> Some
            
    let tryMoveRight zipper =
        match zipper.Right with
        | [] -> None
        | head :: tail ->
            {
                Left = zipper.Focus :: zipper.Left
                Focus = head
                Right = tail
            } |> Some
            
    let selfAndRights zipper =
        let gen = function
        | None -> None
        | Some z -> Some (z, tryMoveRight z)
        List.unfold gen (Some zipper)

    let map (f : 'a -> 'b) (zipper : Zipper<'a>) : Zipper<'b> =
        create (zipper.Left |> List.map f) (f zipper.Focus) (zipper.Right |> List.map f)

    let toList (z : Zipper<'a>) : List<'a> =
        (List.rev z.Left) @ [z.Focus] @ z.Right
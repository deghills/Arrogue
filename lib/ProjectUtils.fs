namespace Rogue.Lib

module ProjectUtils =

    let flip f a b = f b a
    let konst x _ = x

    module String =
        let singleton = string: char -> string

    module Option =
        let toResult (errorWhenNone: 'error) = function
            | Some x -> Ok x
            | None -> Error errorWhenNone

    module ArrayJagged =
        let init rows columns initializer =
            [| for i in 0..rows - 1 ->
                [| for j in 0..columns - 1 -> initializer i j |]
            |]

        let row i (arr: 'a array array) =
            arr[i]

        let map (f: 'a -> 'b) = (Array.map >> Array.map) f

        let mapi (f: int -> int -> 't -> 'u) (arr: 't array array) =
            Array.mapi
                (fun i ->
                    Array.mapi
                        (fun j t ->
                            f i j t
                        )
                )
                arr

        let set (i, j) newValue =
            mapi
                (fun i' j' oldValue ->
                    if i = i' && j = j' then newValue else oldValue)
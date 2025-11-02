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

        let toList = function
            | Some x -> [x]
            | None -> []
namespace Rogue.Lib

module ProjectUtils =

    let flip f a b = f b a
    let konst x _ = x

    module Seq =
        let (|Cons|Nil|) (xs: 'a seq) =
            match Seq.tryHead xs with
            | Some head -> Cons (head, Seq.skip 1 xs)
            | None -> Nil

    module String =
        let singleton = string: char -> string

    module Option =
        let toResult (errorWhenNone: 'error) = function
            | Some x -> Ok x
            | None -> Error errorWhenNone

        let toList = function
            | Some x -> [x]
            | None -> []
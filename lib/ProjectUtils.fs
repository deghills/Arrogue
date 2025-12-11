namespace Rogue.Lib

open System

module ProjectUtils =
    let flip f a b = f b a
    let konst x _ = x
    let s unary binary x = binary x (unary x)
    let s' unaryLeft unaryRight binary x = binary (unaryLeft x) (unaryRight x)

    let (|Positive|Negative|Zero|) x =
        if x = 0 then Zero
        elif x > 0 then Positive
        else Negative

    [<RequireQualifiedAccess>]
    module Lens =
        type t<'structure, 'focus> = { get: 'structure -> 'focus; update: ('focus -> 'focus) -> 'structure -> 'structure }
        let compose { get = leftGet; update = leftSet } { get = rightGet; update = rightSet } =
            { get = leftGet >> rightGet; update = rightSet >> leftSet }

    module RandomPure =
        let getNext { Lens.get = get; Lens.update = update } structure =
            let seed' =
                //Xorshift
                get structure
                |> s' id (flip (<<<) 13) (^^^)
                |> s' id (flip (>>>) 17) (^^^)
                |> s' id (flip (<<<) 5) (^^^)
            let structure' = update (konst seed') structure
            structure', seed'

    module Seq =
        let (|Cons|Nil|) xs =
            match Seq.tryHead xs with
            | Some head -> Cons (head, Seq.skip 1 xs)
            | None -> Nil

        let Cons (x, xs) = seq { yield x; yield! xs }
        let Nil = Seq.empty

        let tryMinBy projection source =
            try Seq.minBy projection source |> Some
            with :? ArgumentException -> None

    module String =
        let singleton = string: char -> string

        let ofSeq: char seq -> String =
            Seq.map string
            >> String.concat ""

    module Option =
        let toResult (errorWhenNone: 'error) = function
            | Some x -> Ok x
            | None -> Error errorWhenNone
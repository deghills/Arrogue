namespace Rogue.Lib

open System

module ProjectUtils =

    let flip f a b = f b a
    let konst x _ = x

    let inline (|Positive|Negative|Zero|) x =
        if x = 0 then Zero
        elif x > 0 then Positive
        else Negative

    type Random with
        member this.CoinFlip() =
            this.Next(Int32.MinValue, Int32.MaxValue)
            |> Int32.IsPositive

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
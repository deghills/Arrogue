namespace Rogue.Lib

open System

module ProjectUtils =
    let flip f a b = f b a
    let konst x _ = x
    let s' unaryLeft binary unaryRight x = binary (unaryLeft x) (unaryRight x)

    let betterModulo i m =
        ((i % m) + m) % m

    let (|Positive|Negative|Zero|) x =
        if x = 0 then Zero
        elif x > 0 then Positive
        else Negative

    type Random with
        member this.CoinFlip() =
            this.Next() > 0

    [<RequireQualifiedAccess>]
    module Lens =
        type t<'structure, 'focus> =
            { get: 'structure -> 'focus
            ; update: ('focus -> 'focus) -> 'structure -> 'structure
            } member this.set x = this.update (konst x)

        let compose { get = leftGet; update = leftSet } { get = rightGet; update = rightSet } =
            { get = leftGet >> rightGet; update = rightSet >> leftSet }

        let identity = { get = id; update = id }

    module State =
        type t<'s, 'a> = { RunState : 's -> 's * 'a }

        let return_ x = { RunState = fun s -> s, x }

        let map mapping { RunState = runState } =
            { RunState = runState >> function s, x -> s, mapping x }

        let bind (binding: 'a -> t<'s, 'b>) { RunState = runState : 's -> 's * 'a} =
            { RunState =
                runState
                >> function s, x -> (binding x).RunState s
            }

        let get = { RunState = fun s -> s, s }
        let put x = { RunState = fun _ -> x, () }
        let modify f = { RunState = fun s -> f s, () }
        
        type StateBuilder() =
            member _.Bind(x, f) = bind f x
            member _.Return x = return_ x
            member _.ReturnFrom m = bind return_ m
            member _.Zero() = return_ ()

        let state = StateBuilder()

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

        /// input is modulo the size of the sequence
        let itemModulo i (xs: seq<'x>) =
            Seq.item
                (betterModulo i (Seq.length xs))
                xs

    module String =
        let singleton = string: char -> string

        let ofSeq: char seq -> String =
            Seq.map string
            >> String.concat ""

    module Option =
        let toResult (errorWhenNone: 'error) = function
            | Some x -> Ok x
            | None -> Error errorWhenNone
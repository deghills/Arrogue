namespace Rogue.Lib

open System
open Accessor

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

    module Map =
        let itemLens key map =
            { Get = Map.tryFind key map
            ; Change = fun f -> Map.change key f map }

        let fromKeyProjection (projection: 't -> 'key) (values: seq<'t>) =
            values
            |> Seq.map (fun value -> projection value, value)
            |> Map

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

    module List =
        let prepend x xs = x :: xs

    module String =
        let singleton = string: char -> string

        let ofSeq: char seq -> String =
            Seq.map string
            >> String.concat ""

    module Option =
        type OptionCEBuilder() =
            member _.Bind(x, k) = Option.bind k x
            member _.Return x = Some x
            member _.Zero() = None

            member _.Delay (f: unit -> Option<'t>) = f()
            member _.Combine (a: Option<'t>, b: Option<'u>) =
                a |> Option.bind (fun _ -> b)

            member _.ReturnFrom m = Option.bind Some m

        let option = OptionCEBuilder()

        let toResult (errorWhenNone: 'error) = function
            | Some x -> Ok x
            | None -> Error errorWhenNone

    module Result =
        let merge (f: 'x -> 'y) (r: Result<'x, 'x>) =
            match r with Ok x | Error x -> f x

        type ResultCEBuilder() =
            member _.Bind(x, k) = Result.bind k x
            member _.Return x = Ok x
            member _.Zero() = Ok ()

            member _.Delay (f: unit -> Result<'t, 'err>) = f()
            member _.Combine(a: Result<'T, 'Error>, b: Result<'U, 'Error>) =
                a |> Result.bind (fun _ -> b)

            member _.ReturnFrom m = Result.bind Ok m

        let result = ResultCEBuilder()

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

    module Writer =
        type Writer<'s, 'a> =
            | Writer of ('a * List<'s>)

        let return_ x = Writer (x, [])
        let map mapping = function Writer (history, value) -> Writer (history, mapping value)
        let bind binding = function
            Writer (oldValue, oldHistory) ->
                match binding oldValue with
                Writer (newValue, newHistory) -> Writer (newValue, oldHistory @ newHistory)

        let write h = Writer ((), [h])
        let writeMany hs = Writer ((), hs)
        let unwrap (Writer w) = w

        type WriterCEBuilder() =
            member _.Bind(x, k) = bind k x
            member _.Return x = return_ x
            member _.Zero() = return_ ()

            member _.Delay (f: unit -> Writer<'h, 't>) = f()
            
            member _.Combine(a: Writer<'h, 't>, b: Writer<'h, 'u>) =
                a |> bind (fun _ -> b)

            member this.For(xs: seq<'x>, k: 'x -> Writer<'h, 'k>) =
                xs
                |> Seq.map k
                |> Seq.reduce (fun a b -> this.Combine(a, b))

        let writer = WriterCEBuilder()
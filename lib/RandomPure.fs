namespace Rogue.Lib

module RandomPure =

    open System
    open ProjectUtils

    type RandomSeed =
        | Seed of int
        member this.Value = match this with Seed value -> ValueNone

    let xorShift (Seed seed) =
        seed
        |> s' id (^^^) (flip (<<<) 13)
        |> s' id (^^^) (flip (>>>) 17)
        |> s' id (^^^) (flip (<<<) 5)
        |> Seed

    let next =
        { State.RunState = function
            seed -> xorShift seed, seed }

    let nextInRange (minBounds: int) (maxBounds: int) =
        if minBounds > maxBounds then raise (ArgumentOutOfRangeException($"{minBounds} {maxBounds}"))

        if minBounds = maxBounds
        then State.return_ (Seed minBounds)
        else State.map
                (fun (Seed seed) ->
                    (betterModulo
                        seed
                        (maxBounds - minBounds)
                    + minBounds)
                    |> Seed
                )
                next

    let coinFlip = State.map (function Seed seed -> seed > 0) next

    let randomItem xs =
        State.state {
            let! (Seed index) = nextInRange 0 (Seq.length xs)
            return Seq.item index xs
        }
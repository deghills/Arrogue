namespace Rogue.Lib

module RandomPure =

    open System
    open ProjectUtils

    type seed = { Seed : int }
    let intLens = { Lens.get = _.Seed; Lens.update = fun f r -> { r with Seed = f r.Seed }}

    let xorShift =
        (  s' id (^^^) (flip (<<<) 13)
        >> s' id (^^^) (flip (>>>) 17)
        >> s' id (^^^) (flip (<<<) 5)
        ) |> intLens.update

    let next =
        { State.RunState =
            fun structure ->
                xorShift structure, structure.Seed
        }

    let nextInRange minBounds maxBounds =
        if minBounds > maxBounds then raise (ArgumentOutOfRangeException($"{minBounds} {maxBounds}"))

        if minBounds = maxBounds
        then State.return_ minBounds
        else State.map
                (fun seed ->
                    betterModulo
                        seed
                        (maxBounds - minBounds)
                    + minBounds
                )
                next

    let coinFlip = State.map ((<) 0) next

    let randomItem xs =
        State.state {
            let! index = nextInRange 0 (Seq.length xs)
            return Seq.item index xs
        }
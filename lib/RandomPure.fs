namespace Rogue.Lib

module RandomPure =

    open System
    open ProjectUtils

    type RandomSeed = Seed of int

    let xorShift =
        (  s' id (^^^) (flip (<<<) 13)
        >> s' id (^^^) (flip (>>>) 17)
        >> s' id (^^^) (flip (<<<) 5)
        )

    let next =
        { State.RunState = function
            Seed seed -> Seed (xorShift seed), seed }

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
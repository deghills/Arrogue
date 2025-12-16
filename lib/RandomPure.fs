namespace Rogue.Lib

module RandomPure =

    open System
    open ProjectUtils

    type seed = { Seed : int }
    let intLens = { Lens.get = _.Seed; Lens.update = fun f r -> { r with Seed = f r.Seed }}

    let xorShift =
        (  s' id (flip (<<<) 13) (^^^)
        >> s' id (flip (>>>) 17) (^^^)
        >> s' id (flip (<<<) 5) (^^^)
        ) |> intLens.update

    type Rand<'structure> (randLens: Lens.t<'structure, seed>) =
        member _.Next =
            { State.RunState =
                fun structure ->
                    let oldSeed = randLens.get structure
                    randLens.update xorShift structure, oldSeed.Seed
            }

        member this.NextInRange minBounds maxBounds =
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
                    this.Next

        member this.CoinFlip = State.map ((<) 0) this.Next

        member this.RandomItem xs =
            State.state {
                let! index = this.NextInRange 0 (Seq.length xs)
                return Seq.item index xs
            }
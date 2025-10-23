module Block

open System

type t =
    val chars : Map<int*int, char>

    new (chars) = { chars = chars }

    new (width, height, initializer: int -> int -> char) =
        let chars =
            seq {
                for row in 0..height-1 do
                    for col in 0..width-1 do
                        match initializer row col with
                        | ' ' -> ()
                        | chr -> yield ((row, col), chr)
            } |> Map
        { chars = chars }

    new (chars: string seq) =
        let acc = Collections.Generic.Dictionary<int*int, char>()
        let () =
            Seq.iteri
                (fun i ->
                    Seq.iteri
                        (fun j chr -> if chr <> ' ' then acc.Add((j,i), chr))
                )
                chars

        let chars' =
            acc
            |> Seq.map (function KeyValue (k, v) -> k, v)
            |> Map

        t (chars')

    member this.GetChr (i, j) =
        this.chars
        |> Map.tryFind (i, j)
        |> Option.defaultValue ' '

    member this.SetChr idx chr =
        this.chars
        |> Map.add idx chr

    member this.Item =
        this.GetChr
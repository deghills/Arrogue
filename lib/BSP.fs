module BSP

open System
open Rogue.Lib
open ProjectUtils

[<RequireQualifiedAccess>]
module Bisector =
    type t = Horizontal of int | Vertical of int

[<RequireQualifiedAccess>]
module Bounds =
    type t (minX, maxX, minY, maxY) =
        member _.MinX = min minX maxX
        member _.MinY = min minY maxY
        member _.MaxX = max minX maxX
        member _.MaxY = max minY maxY
        member this.Width = this.MaxX - this.MinX
        member this.Height = this.MaxY - this.MinY

    let split bisector (bounds: t) =
        let (|OutOfBounds|_|) = function
            | Bisector.Horizontal translation when translation <= bounds.MinY || translation >= bounds.MaxY -> true
            | Bisector.Vertical translation when translation <= bounds.MinX || translation >= bounds.MaxX -> true
            | _ -> false

        match bisector with
        | OutOfBounds -> SplitTree.Leaf bounds

        | Bisector.Horizontal translation ->
            SplitTree.Branch
                ( SplitTree.Leaf (t (bounds.MinX, bounds.MaxX, bounds.MinY, translation))
                , bisector
                , SplitTree.Leaf (t (bounds.MinX, bounds.MaxX, translation, bounds.MaxY))
                )

        | Bisector.Vertical translation ->
            SplitTree.Branch
                ( SplitTree.Leaf (t (bounds.MinX, translation, bounds.MinY, bounds.MaxY))
                , bisector
                , SplitTree.Leaf (t (translation, bounds.MaxX, bounds.MinY, bounds.MaxY))
                )

    let containsPoint (Vec (x, y)) (bounds: t) =
        bounds.MinX <= x && x < bounds.MaxX &&
        bounds.MinY <= y && y < bounds.MaxY

    let containedPoints (bounds: t) =
        seq
            { for i in bounds.MinX .. bounds.MaxX - 1 do
                for j in bounds.MinY .. bounds.MaxY - 1 do
                    yield Vec(i, j)
            }

    let randomPointWithin (bounds: t) =
        let rand = Random()
        ( rand.Next(bounds.MinX, bounds.MaxX)
        , rand.Next(bounds.MinY, bounds.MaxY)
        ) |> Vec

    let smallestBindingPoints (points: IntVec list) =
        let minX = points |> Seq.map _.X |> Seq.min
        let minY = points |> Seq.map _.Y |> Seq.min
        let maxX = points |> Seq.map _.X |> Seq.max
        let maxY = points |> Seq.map _.Y |> Seq.max

        t (minX, maxX + 1, minY, maxY + 1)

let connect (leftRoom: List<Bounds.t>) (rightRoom: List<Bounds.t>) =
    let leftAnchor, rightAnchor =
        ( leftRoom |> Seq.randomChoice |> Bounds.randomPointWithin
        , rightRoom |> Seq.randomChoice |> Bounds.randomPointWithin
        )

    let intersect = Vec (leftAnchor.X, rightAnchor.Y)

    [ yield! leftRoom
    ; yield! rightRoom
    ; yield Bounds.smallestBindingPoints [leftAnchor; intersect]
    ; yield Bounds.smallestBindingPoints [rightAnchor; intersect]
    ]

let split n tree =
    let rec aux remaining t =
        match remaining, t with
        | Positive, SplitTree.Branch (l, bisector, r) ->
            SplitTree.Branch (aux remaining l, bisector, aux remaining r)

        | Positive, SplitTree.Leaf (bounds: Bounds.t) ->
            let width, height = bounds.MaxX - bounds.MinX, bounds.MaxY - bounds.MinY
            aux
                (remaining - 1)
                (if width > height
                    then Bounds.split (Bisector.Vertical (bounds.MinX + width / 2)) bounds
                    else Bounds.split (Bisector.Horizontal (bounds.MinY + height / 2)) bounds
                )

        | _ -> t
    in aux n tree

let splitRandom minSize n tree =
    let rec aux remaining t =
        match remaining, t with
        | Positive, SplitTree.Branch (l, bisector, r) ->
            SplitTree.Branch (aux remaining l, bisector, aux remaining r)

        | Positive, SplitTree.Leaf (bounds: Bounds.t) ->
            let rand = Random()
            try
                if rand.CoinFlip() then
                    Bounds.split
                        (rand.Next(bounds.MinX + minSize, bounds.MaxX - minSize) |> Bisector.Vertical)
                        bounds
                else
                    Bounds.split
                        (rand.Next(bounds.MinY + minSize, bounds.MaxY - minSize) |> Bisector.Horizontal)
                        bounds
            with
                | :? ArgumentOutOfRangeException -> SplitTree.Leaf bounds
                | exn -> raise exn
            |> aux (remaining - 1)

        | _ -> t
    in aux n tree

let shrink n (bounds: Bounds.t) =
    Bounds.t (bounds.MinX + n, bounds.MaxX - n, bounds.MinY + n, bounds.MaxY - n)

let randomSubroom minSize (bsp: SplitTree.t<Bisector.t, Bounds.t>) =
    let makeBoundsSmaller (bounds: Bounds.t) =
        let rand = Random()
        let width' = rand.Next(minSize, bounds.Width)
        let height' = rand.Next(minSize, bounds.Height)

        let minX = rand.Next(bounds.MinX, bounds.MaxX - width')
        let minY = rand.Next(bounds.MinY, bounds.MaxY -  height')
        let maxX = minX + width'
        let maxY = minY + height'

        Bounds.t (minX, maxX, minY, maxY)
    
    bsp
    |> SplitTree.map makeBoundsSmaller

let buildRandomPaths (bsp: SplitTree.t<Bisector.t, Bounds.t>) =
    bsp
    |> SplitTree.map List.singleton
    |> SplitTree.interpret (konst connect)

let genRandomMap startingBounds minRoomSize divisions =
    let minRoomSize = minRoomSize + 2
    startingBounds
    |> SplitTree.Leaf
    |> splitRandom minRoomSize divisions
    |> randomSubroom minRoomSize
    |> SplitTree.map (shrink 1)
    |> buildRandomPaths
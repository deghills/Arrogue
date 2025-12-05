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

    let randomPointWithin (bounds: t) =
        let rand = Random()
        ( rand.Next(bounds.MinX, bounds.MaxX)
        , rand.Next(bounds.MinY, bounds.MaxY)
        ) |> Vec

let split n tree =
    let rec aux remaining t =
        match remaining, t with
        | Positive, SplitTree.Branch (l, bounds, r) ->
            SplitTree.Branch (aux remaining l, bounds, aux remaining r)

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

let splitRandom n minSize tree =
    let rec aux remaining t =
        match remaining, t with
        | Positive, SplitTree.Branch (l, bounds, r) ->
            SplitTree.Branch (aux remaining l, bounds, aux remaining r)

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

let connect (leftRoom: List<Bounds.t>) (rightRoom: List<Bounds.t>) =
    let leftAnchor, rightAnchor =
        leftRoom |> Seq.randomChoice |> Bounds.randomPointWithin,
        rightRoom |> Seq.randomChoice |> Bounds.randomPointWithin

    let getPathBounds (Vec (leftX, leftY)) (Vec (rightX, rightY)) =
        [ Bounds.t (leftX, rightX, leftY, leftY + 1)
        ; Bounds.t (rightX - 1, rightX, leftY, rightY)
        ]

    [ yield! leftRoom
    ; yield! rightRoom
    ; yield! getPathBounds leftAnchor rightAnchor
    ]

let buildRandomPaths (bsp: SplitTree.t<Bisector.t, Bounds.t>) =
    bsp
    |> SplitTree.map List.singleton
    |> SplitTree.interpret (konst connect)

let toWalls (bsp: seq<Bounds.t>) =
    let isVacant vec =
        Seq.forall (not << Bounds.containsPoint vec) bsp

    let (Vec (mostMinX, mostMinY)) =
        bsp
        |> Seq.map (fun bounds -> Vec (bounds.MinX, bounds.MinY))
        |> Seq.min
        |> fun v -> v - Vec (1, 1)

    let (Vec (mostMaxX, mostMaxY)) =
        bsp
        |> Seq.map (fun bounds -> Vec (bounds.MaxX, bounds.MaxY))
        |> Seq.max

    [ for i in mostMinX .. mostMaxX do
        for j in mostMinY .. mostMaxY do
            let tile = Vec (i, j) in if tile |> isVacant then yield tile
    ]
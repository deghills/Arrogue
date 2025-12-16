namespace Rogue.Lib

module BSP =

    //open System
    open Rogue.Lib
    open ProjectUtils

    [<RequireQualifiedAccess>]
    module Bisector =
        type Bisector = Horizontal of int | Vertical of int

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

        let smallestBindingPoints (points: IntVec list) =
            let minX = points |> Seq.map _.X |> Seq.min
            let minY = points |> Seq.map _.Y |> Seq.min
            let maxX = points |> Seq.map _.X |> Seq.max
            let maxY = points |> Seq.map _.Y |> Seq.max

            t (minX, maxX + 1, minY, maxY + 1)

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

    let shrink n (bounds: Bounds.t) =
        Bounds.t (bounds.MinX + n, bounds.MaxX - n, bounds.MinY + n, bounds.MaxY - n)

    let randomPointWithinBounds (bounds: Bounds.t) =
        State.state {
            let! x = RandomPure.nextInRange bounds.MinX bounds.MaxX
            let! y = RandomPure.nextInRange bounds.MinY bounds.MaxY

            return Vec (x, y)
        }
        
    let randomConnect (leftRoom: List<Bounds.t>) (rightRoom: List<Bounds.t>) =
        State.state {
            let! leftAnchor =
                RandomPure.randomItem leftRoom
                |> State.bind randomPointWithinBounds

            let! rightAnchor =
                RandomPure.randomItem rightRoom
                |> State.bind randomPointWithinBounds

            let intersect = Vec (leftAnchor.X, rightAnchor.Y)
            
            return
                [ yield! leftRoom
                ; yield! rightRoom
                ; yield Bounds.smallestBindingPoints [leftAnchor; intersect]
                ; yield Bounds.smallestBindingPoints [rightAnchor; intersect]
                ]
        }

    let randomSplit minSize n tree =
        let rec aux remaining t =
            match remaining, t with
            | Positive, SplitTree.Branch (l, bisector, r) ->
                State.state {
                    let! l' = aux remaining l
                    let! r' = aux remaining r

                    return SplitTree.Branch (l', bisector, r')
                }

            | Positive, SplitTree.Leaf (bounds: Bounds.t) ->
                State.state {
                    let! headsOrTails = RandomPure.coinFlip

                    let! x =
                        try
                            if headsOrTails then
                                State.state {
                                    let! splitPos = RandomPure.nextInRange (bounds.MinX + minSize) (bounds.MaxX - minSize)
                                    return Bounds.split (Bisector.Vertical splitPos) bounds
                                }

                            else
                                State.state {
                                    let! splitPos = RandomPure.nextInRange (bounds.MinY + minSize) (bounds.MaxY - minSize)
                                    return Bounds.split (Bisector.Horizontal splitPos) bounds
                                }
                        with _ -> State.return_ (SplitTree.Leaf bounds)

                    return! (aux (remaining - 1) x)
                }

            | _ -> State.return_ t
        in aux n tree

    let randomSubroom minSize (bsp: SplitTree.t<Bisector.Bisector, Bounds.t>) =
        let makeBoundsSmaller (bounds: Bounds.t) =
            State.state {
                let! width = RandomPure.nextInRange minSize bounds.Width
                let! height = RandomPure.nextInRange minSize bounds.Height
                let! minX = RandomPure.nextInRange bounds.MinX (bounds.MaxX - width)
                let! minY = RandomPure.nextInRange bounds.MinY (bounds.MaxY - height)
                let maxX = minX + width
                let maxY = minY + height
                
                return Bounds.t (minX, maxX, minY, maxY)
            }

        let rec mapWithState (mapping: 'a -> State.t<'state, 'b>) : SplitTree.t<'bisector, 'a> -> State.t<'state, SplitTree.t<'bisector, 'b>> = function
            | SplitTree.Branch (l: SplitTree.t<'bisector, 'a>, bounds: 'bisector, r: SplitTree.t<'bisector, 'a>) ->
                State.state {
                    let! l' = mapWithState mapping l
                    let! r' = mapWithState mapping r
                    return SplitTree.Branch (l', bounds, r')
                }

            | SplitTree.Leaf x -> State.state { return! mapping x |> State.map SplitTree.Leaf }

        in mapWithState makeBoundsSmaller bsp

    let randomPaths (bsp: SplitTree.t<Bisector.Bisector, Bounds.t>) =
        bsp
        |> SplitTree.map (List.singleton >> State.return_)
        |> SplitTree.interpret
            (fun _ a b -> 
                State.state {
                    let! a = a
                    let! b = b
                    return! randomConnect a b
                }
            )

    let genRandomMap startingBounds minRoomSize divisions =
        let minRoomSize = minRoomSize + 2

        startingBounds
        |> SplitTree.Leaf
        |> randomSplit minRoomSize divisions
        |> State.bind (randomSubroom minRoomSize)
        |> (SplitTree.map >> State.map) (shrink 1)
        |> State.bind randomPaths
        |> State.map (Seq.collect Bounds.containedPoints)
        |> State.map Set
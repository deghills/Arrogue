namespace Rogue.Lib

module BSP =

    //open System
    open Rogue.Lib
    open ProjectUtils
    open RandomPure

    type Bisector = Horizontal of int | Vertical of int

    module Bounds =
        let split bisector (bounds: Bounds) =
            let (|OutOfBounds|_|) = function
                | Bisector.Horizontal translation when translation <= bounds.MinY || translation >= bounds.MaxY -> true
                | Bisector.Vertical translation when translation <= bounds.MinX || translation >= bounds.MaxX -> true
                | _ -> false

            match bisector with
            | OutOfBounds -> SplitTree.Leaf bounds

            | Bisector.Horizontal translation ->
                SplitTree.Branch
                    ( SplitTree.Leaf (Bounds (bounds.MinX, bounds.MaxX, bounds.MinY, translation))
                    , bisector
                    , SplitTree.Leaf (Bounds (bounds.MinX, bounds.MaxX, translation, bounds.MaxY))
                    )

            | Bisector.Vertical translation ->
                SplitTree.Branch
                    ( SplitTree.Leaf (Bounds (bounds.MinX, translation, bounds.MinY, bounds.MaxY))
                    , bisector
                    , SplitTree.Leaf (Bounds (translation, bounds.MaxX, bounds.MinY, bounds.MaxY))
                    )


    let split n tree =
        let rec aux remaining t =
            match remaining, t with
            | Positive, SplitTree.Branch (l, bisector, r) ->
                SplitTree.Branch (aux remaining l, bisector, aux remaining r)

            | Positive, SplitTree.Leaf (bounds: Bounds) ->
                let width, height = bounds.MaxX - bounds.MinX, bounds.MaxY - bounds.MinY
                aux
                    (remaining - 1)
                    (if width > height
                        then Bounds.split (Bisector.Vertical (bounds.MinX + width / 2)) bounds
                        else Bounds.split (Bisector.Horizontal (bounds.MinY + height / 2)) bounds
                    )

            | _ -> t
        in aux n tree

    let shrink n (bounds: Bounds) =
        Bounds (bounds.MinX + n, bounds.MaxX - n, bounds.MinY + n, bounds.MaxY - n)

    let randomPointWithinBounds (bounds: Bounds) =
        State.state {
            let! (Seed x) = RandomPure.nextInRange bounds.MinX bounds.MaxX
            let! (Seed y) = RandomPure.nextInRange bounds.MinY bounds.MaxY

            return Vec (x, y)
        }
        
    let randomConnect (leftRoom: List<Bounds>) (rightRoom: List<Bounds>) =
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
                ; yield Bounds.SmallestBindingPoints [leftAnchor; intersect]
                ; yield Bounds.SmallestBindingPoints [rightAnchor; intersect]
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

            | Positive, SplitTree.Leaf (bounds: Bounds) ->
                State.state {
                    let! headsOrTails = RandomPure.coinFlip

                    let! x =
                        try
                            if headsOrTails then
                                State.state {
                                    let! (Seed splitPos) = RandomPure.nextInRange (bounds.MinX + minSize) (bounds.MaxX - minSize)
                                    return Bounds.split (Bisector.Vertical splitPos) bounds
                                }

                            else
                                State.state {
                                    let! (Seed splitPos) = RandomPure.nextInRange (bounds.MinY + minSize) (bounds.MaxY - minSize)
                                    return Bounds.split (Bisector.Horizontal splitPos) bounds
                                }
                        with _ -> State.return_ (SplitTree.Leaf bounds)

                    return! (aux (remaining - 1) x)
                }

            | _ -> State.return_ t
        in aux n tree

    let randomSubroom minSize (bsp: SplitTree.t<Bisector, Bounds>) =
        let makeBoundsSmaller (bounds: Bounds) =
            State.state {
                let! (Seed width) = RandomPure.nextInRange minSize bounds.Width
                let! (Seed height) = RandomPure.nextInRange minSize bounds.Height
                let! (Seed minX) = RandomPure.nextInRange bounds.MinX (bounds.MaxX - width)
                let! (Seed minY) = RandomPure.nextInRange bounds.MinY (bounds.MaxY - height)
                let maxX = minX + width
                let maxY = minY + height
                
                return Bounds (minX, maxX, minY, maxY)
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

    let randomPaths (bsp: SplitTree.t<Bisector, Bounds>) =
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
        |> State.map (Seq.collect Bounds.ContainedPoints)
        |> State.map Set
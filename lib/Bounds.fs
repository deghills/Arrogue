namespace Rogue.Lib

type Bounds (minX, maxX, minY, maxY) =
    member _.MinX = min minX maxX
    member _.MinY = min minY maxY
    member _.MaxX = max minX maxX
    member _.MaxY = max minY maxY
    member this.Width = this.MaxX - this.MinX
    member this.Height = this.MaxY - this.MinY


    static member ContainsPoint (Vec (x, y)) (bounds: Bounds) =
        bounds.MinX <= x && x < bounds.MaxX &&
        bounds.MinY <= y && y < bounds.MaxY

    static member ContainedPoints (bounds: Bounds) =
        seq
            { for i in bounds.MinX .. bounds.MaxX - 1 do
                for j in bounds.MinY .. bounds.MaxY - 1 do
                    yield Vec(i, j)
            }

    static member SmallestBindingPoints (points: IntVec list) =
        let minX = points |> Seq.map _.X |> Seq.min
        let minY = points |> Seq.map _.Y |> Seq.min
        let maxX = points |> Seq.map _.X |> Seq.max
        let maxY = points |> Seq.map _.Y |> Seq.max

        Bounds (minX, maxX + 1, minY, maxY + 1)
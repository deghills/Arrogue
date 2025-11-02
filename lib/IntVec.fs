namespace Rogue.Lib

[<Struct>]
type IntVec =
    | Vec of int*int
    static member (+) (Vec (x, y), Vec (i, j)) = Vec (x + i, y + j)
    static member (~-) (Vec (x, y)) = Vec (-x, -y)
    static member (-) (lhs: IntVec, rhs: IntVec) = lhs + -rhs
    member this.X = match this with Vec (x, _) -> x
    member this.Y = match this with Vec (_, y) -> y

    /// Chebyshev norm
    static member NormChebyshev (Vec (x, y)) = max (abs x) (abs y)

    /// Manhattan norm
    static member NormManhattan (Vec (x, y)) = abs x + abs y

    static member NormEuclidean (Vec (x, y)) =
        let sqr f = f * f
        System.MathF.Sqrt
            ( sqr (float32 x)
            + sqr (float32 y) )

    // Taxi-cab norm
    //static member Norm = function Vec (x, y) -> x + y

    static member Zero = Vec (0, 0)
    static member Up = Vec (0, -1)
    static member Down = Vec (0, 1)
    static member Right = Vec (1, 0)
    static member Left = Vec (-1, 0)
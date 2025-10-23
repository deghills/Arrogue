module IntVec

[<Struct>]
type t =
    | Vec of int*int
    static member (+) (Vec (x, y), Vec (i, j)) =
        Vec (x + i, y + j)
    static member (~-) (Vec (x, y)) = Vec (-x, -y)
    static member (-) (lhs: t, rhs: t) = lhs + -rhs
    member this.X = match this with Vec (x, _) -> x
    member this.Y = match this with Vec (_, y) -> y
    /// Chebyshev norm
    member this.Norm = match this with Vec (x, y) -> max (abs x) (abs y)

/// Chebyshev norm
let norm: t -> int = _.Norm

let zero = Vec (0, 0)
let up = Vec (0, -1)
let down = Vec (0, 1)
let right = Vec (1, 0)
let left = Vec (-1, 0)
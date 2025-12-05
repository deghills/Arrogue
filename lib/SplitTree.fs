namespace Rogue.Lib

open ProjectUtils

/// Split tree is a binary tree where nodes have exactly 0 or 2 children
[<RequireQualifiedAccess>]
module SplitTree =
    type t<'bisector, 't> =
        | Leaf of 't
        | Branch of t<'bisector, 't> * 'bisector * t<'bisector, 't>
        interface seq<'t> with
            member this.GetEnumerator (): System.Collections.Generic.IEnumerator<'t> = 
                let rec aux acc = function
                    | Seq.Nil -> acc
            
                    | Seq.Cons (Leaf x, queue) ->
                        aux (Seq.Cons (x, acc)) queue

                    | Seq.Cons (Branch (l, _, r), queue) ->
                        aux acc (seq { yield l; yield r; yield! queue })

                in aux Seq.empty (Seq.singleton this) |> _.GetEnumerator()

            member this.GetEnumerator (): System.Collections.IEnumerator = 
                this :> seq<'t> |> _.GetEnumerator()

    let map mapping =
        let rec aux = function
            | Leaf x -> (Leaf (mapping x))
            | Branch (l, x, r) -> Branch (aux l, x, aux r)
        in aux

    let bind kleisli =
        let rec aux = function
            | Leaf x -> kleisli x
            
            | Branch (l, x, r) ->
                Branch (aux l, x, aux r)
        in aux

    let toList tree =
        let rec aux acc = function
            | [] -> acc
            
            | Leaf x :: queue ->
                aux (x :: acc) queue

            | Branch (l, _, r) :: queue ->
                aux acc (l :: r :: queue)

        in aux [] [tree]

    let interpret (interpreter: 'bisector -> 't -> 't -> 't) =
        let rec aux = function
            | Leaf x -> x
        
            | Branch (left, bisector, right) ->
                interpreter bisector (aux left) (aux right)
        in aux
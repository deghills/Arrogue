namespace Rogue.Lib.Graph

open Rogue.Lib

///undirected, unweighted
[<Struct>]
type Edge<'vertex when 'vertex : comparison> =
    val internal vertices : ('vertex * 'vertex)
    
    new (v1, v2) = { vertices = if v1 > v2 then (v2, v1) else (v1, v2) }
    
    member this.ContainsVertex (v: 'vertex) =
        let (v1, v2) = this.vertices in (v = v1) || (v = v2)

    member this.CheckComplement (v: 'vertex) =
        let (|V|_|) = (=) v
        match this.vertices with
        | complement, V | V, complement -> Some complement
        | _ -> None

type Simple<'vertex when 'vertex : comparison> (edges: Set<Edge<'vertex>>) =
    member _.RemoveVertex (v: 'vertex) =
        edges
        |> Set.filter (_.ContainsVertex(v) >> not)
        |> Simple

    member _.GetNeighbours (v: 'vertex) =
        edges
        |> Seq.choose _.CheckComplement(v)
        |> Set

    member _.Edges =
        edges
    member _.Vertices =
        edges
        |> Seq.collect (_.vertices >> function (v1, v2) -> seq { v1; v2 })
        |> Set

    static member Lattice width height =
        seq
            { for i in 0..width - 1 do
                for j in 0..height - 1 do
                    let topLeft = Vec (i, j)
                    let topRight = topLeft + IntVec.Right
                    let bottomLeft = topLeft + IntVec.Down
                    let bottomRight = bottomLeft + IntVec.Right
                    yield Edge (topLeft, topRight)
                    yield Edge (topLeft, bottomRight)
                    yield Edge (topLeft, bottomLeft)
                    yield Edge (bottomLeft, bottomRight)
                    yield Edge (bottomLeft, topRight)
                    yield Edge (topRight, bottomRight)
            }
        |> Set
        |> Simple

    static member Dijkstra (start: 'vertex) (finish: 'vertex) (graph: Simple<'vertex>) =
        let initPriorityQ =
            seq
                { yield!
                    Seq.map
                        (fun x ->
                            (x, (infinityf, None))
                        )
                        graph.Vertices
                ; start, (0f, None)
                }
            |> Map<'vertex, float32*'vertex option>

        let rec step (processedVertices: Map<'vertex, 'vertex>) (queue: Map<'vertex, float32*option<'vertex>>) =

            if processedVertices.ContainsKey finish then
                let rec aux v path =
                    match Map.tryFind v processedVertices with
                    | None when v = start -> path
                    | None -> []
                    | Some previousVertex -> aux previousVertex (v :: path)
                in aux finish []
            else

            let currentVertex =
                Map.toSeq queue
                |> Seq.minBy (function _, (cost, _) -> cost)
                |> fst

            let newDistance = (Map.find currentVertex queue |> fst) + 1f
            
            Set.intersect
                (graph.GetNeighbours currentVertex)
                (Set queue.Keys)
            |> Set.fold
                (fun q unexploredNeighbour ->
                    match Map.find unexploredNeighbour queue with
                    | (oldDistance, _) when newDistance < oldDistance -> Map.add unexploredNeighbour (newDistance, Some currentVertex) q
                    | _ -> q
                )
                queue
            |> fun queue' ->
                match Map.tryFind currentVertex queue |> Option.bind snd with
                | Some v ->
                    step
                        (processedVertices.Add (currentVertex, v))
                        (queue'.Remove currentVertex)
                | None ->
                    step
                        processedVertices
                        (queue'.Remove currentVertex)
            
        in step Map.empty initPriorityQ
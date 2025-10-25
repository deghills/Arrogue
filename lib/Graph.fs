module Graph

open System
open ProjectUtils

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

type Infinity = Infinity

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
                    let topLeft = IntVec.Vec (i, j)
                    let topRight = topLeft + IntVec.right
                    let bottomLeft = topLeft + IntVec.down
                    let bottomRight = bottomLeft + IntVec.right
                    yield Edge (topLeft, topRight)
                    yield Edge (topLeft, bottomRight)
                    yield Edge (topLeft, bottomLeft)
                    yield Edge (bottomLeft, bottomRight)
                    yield Edge (bottomLeft, topRight)
                    yield Edge (topRight, bottomRight)
            }
        |> Set
        |> Simple

    static member Dijkstra (start: 'vertex) (graph: Simple<'vertex>) =
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

        let rec step (processedVertices: Set<'vertex>) (unprocessedVertices: Set<'vertex>) (queue: Map<'vertex, float32*option<'vertex>>) =
            match Set.count unprocessedVertices with
            | 0 ->
                let () = for KeyValue (k, v) in queue do printfn "%A" (k, v)
                fun targetVertex ->
                    let toPreviousVertex =
                        Map.toSeq queue
                        |> Seq.map (function v, (_, previous) -> (v, previous))
                        |> Seq.choose
                            (function
                                | (v, Some previous) -> Some (v, previous)
                                | _ -> None
                            )
                        |> Map
                    let rec aux acc currentVertex =
                        match Map.tryFind currentVertex toPreviousVertex with
                        | None when currentVertex = start -> acc
                        | None -> failwith "there is no path"
                        | Some nextVertex -> aux (currentVertex :: acc) nextVertex
                    in aux [] targetVertex
            | _ ->

            let currentVertex =
                unprocessedVertices
                |> Seq.minBy (fun vertex -> Map.find vertex queue |> fst)

            match
                Set.intersect
                    (graph.GetNeighbours currentVertex)
                    unprocessedVertices
                |> Seq.tryHead
            with
            | None ->
                step
                    (processedVertices.Add currentVertex)
                    (unprocessedVertices.Remove currentVertex)
                    queue
            | Some v ->
                let distanceToV = (Map.find currentVertex queue |> fst) + 1f
                step
                    (processedVertices.Add currentVertex)
                    (unprocessedVertices.Remove currentVertex)
                    (match Map.find v queue |> fst with
                        | oldDistance when distanceToV < oldDistance -> queue.Add (v, (distanceToV, Some currentVertex))
                        | _ -> queue
                    )
            
        in step Set.empty (graph.Vertices) initPriorityQ
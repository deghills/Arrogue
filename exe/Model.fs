module Model

open Rogue.Lib
open ProjectUtils
open Creature

type Model =
    { Creatures: Map<CreatureID, Creature>
    ; Map: Set<IntVec>
    }

    member this.TransformCreatures f = { this with Creatures = f this.Creatures }

    ///Dijkstra/A* (Euclidean norm heuristic)
    member this.FindPath (start: IntVec) (finish: IntVec) =
        let freeTiles = Set this.Map
            
        let getNeighbours (p: IntVec) =
            Set.intersect
                ( Set.empty
                |> Set.add(p + IntVec.Up)
                |> Set.add(p + IntVec.Up + IntVec.Left)
                |> Set.add(p + IntVec.Up + IntVec.Right)
                |> Set.add(p + IntVec.Left)
                |> Set.add(p + IntVec.Right)
                |> Set.add(p + IntVec.Down)
                |> Set.add(p + IntVec.Down + IntVec.Left)
                |> Set.add(p + IntVec.Down + IntVec.Right)
                )
                freeTiles

        let rec aux (visitedTiles: Map<IntVec, IntVec>) (priorityQueue: Map<IntVec, int*option<IntVec>>) =
            let rec checkPath acc current =
                match Map.tryFind current visitedTiles with
                | Some next ->
                    checkPath (current :: acc) next
                | None when current = start ->
                    acc
                | None ->
                    [] //no path exists
            in
            match checkPath [] finish with
            | [] ->
                match
                    priorityQueue
                    |> Map.toSeq
                    (* technically chebyshev norm says that ||(x, 0)|| = ||(x, x)||, which can lead to unnatural looking pathing
                    ** even though it's still technically an optimal path under the chebyshev norm.
                    ** for multiple optimal paths, tie-breaking with the manhattan distance here
                    ** will choose a more natural looking one *)
                    |> Seq.tryMinBy (function pos1, (dist1, _) -> dist1, IntVec.NormManhattan(finish - pos1))
                with
                | None -> [] //queue is empty, no path exists
                | Some (currentTile, (currentDist, previousTileOpt)) ->

                let neighbours =
                    Set.difference
                        (getNeighbours currentTile)
                        (Set visitedTiles.Keys)
                    |> Set.remove start
                let newDist = currentDist + 1 + (*heuristic*)(IntVec.NormChebyshev (finish - currentTile))
                let (|HigherCostDistance|_|) i = i > newDist
            
                aux
                    ( match previousTileOpt with
                    | Some previousTile ->
                        Map.add currentTile previousTile visitedTiles
                    | None ->
                        visitedTiles )

                    ( Set.fold
                        (fun acc neighbour ->
                            match Map.tryFind neighbour acc with
                            | None
                            | Some (HigherCostDistance, _)
                                -> Map.add neighbour (newDist, Some currentTile) acc
                            | _ -> acc
                        )
                        priorityQueue
                        neighbours
                    |> Map.remove currentTile )

            | path ->
                path
        in aux Map.empty (Map.add start (0, None) Map.empty)
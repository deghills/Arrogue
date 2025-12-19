module Model

open Rogue.Lib
open ProjectUtils
open Creature

type EntityID =
    | player = -1

type ComponentStore<'component_> = Map<EntityID, 'component_>
module ComponentStore =
    let empty : ComponentStore<'t> = Map.empty

type GamePiece =
    { Pos: IntVec
    ; Token: char
    }

type Model =
    { Map: Set<IntVec>
    ; Seed: RandomPure.seed

    //components
    ; Tiles: ComponentStore<GamePiece>
    ; Creatures: ComponentStore<Creature>
    }

module Component =
    let tile entityID =
        Map.itemLens entityID

let empty =
    { Tiles = ComponentStore.empty
    ; Creatures = ComponentStore.empty
    ; Map = Set.empty
    ; Seed = { Seed = 69 } }

let gamePiecesL =
    { Lens.get = fun model -> model.Tiles
    ; Lens.change = fun endomorphism model -> { model with Tiles = endomorphism model.Tiles}
    }

let creaturesL =
    { Lens.get = _.Creatures
    ; Lens.change = fun endomorphism model -> { model with Creatures = endomorphism model.Creatures }
    }

let randomL =
    { Lens.get = _.Seed
    ; Lens.change = fun endomorphism model -> { model with Seed = endomorphism model.Seed }
    }

let mapL =
    { Lens.get = _.Map
    ; Lens.change = fun endomorphism model -> { model with Map = endomorphism model.Map }
    }

let spawnCreature entityID gamePiece creature =
    (creaturesL $ Map.itemLens entityID).set (Some creature)
    >> (gamePiecesL $ Map.itemLens entityID).set (Some gamePiece)
    
///Dijkstra/A* (Chebyshev norm heuristic)
let findPath (start: IntVec) (finish: IntVec) model =
    let freeTiles = Set model.Map
            
    let getNeighbours (p: IntVec) =
        Set.intersect
            ( Set.empty
            |> Set.add (p + IntVec.Up)
            |> Set.add (p + IntVec.Up + IntVec.Left)
            |> Set.add (p + IntVec.Up + IntVec.Right)
            |> Set.add (p + IntVec.Left)
            |> Set.add (p + IntVec.Right)
            |> Set.add (p + IntVec.Down)
            |> Set.add (p + IntVec.Down + IntVec.Left)
            |> Set.add (p + IntVec.Down + IntVec.Right)
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
                ** will choose the most natural looking one *)
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

let genNewMap model =
    BSP.genRandomMap (BSP.Bounds.t (0, 64, 0, 32)) 4 4
    |> _.RunState(model.Seed)
    |> fun (newSeed, newMap) ->
        model
        |> randomL.change (konst newSeed)
        |> mapL.change (konst newMap)

(*let spawnCreaturesAtRandom model creatures =
    let rand = RandomPure.Rand(randomLens)

    let getAvailableTiles =
        ProjectUtils.s'
            mapLens.get    
            (  creaturesLens.get
            >> Seq.map (function KeyValue (_, c) -> c.Pos)
            >> Set
            )
            Set.difference

    Seq.fold
        (fun m (creatureID, creature) ->
            State.state {
                let! m = m
                let! pos = rand.RandomItem (getAvailableTiles m)

                let 

                return failwith""
            }
        )
        model
        (Map.toSeq model.Creatures)
    
    let spawn creatureID creature =
        State.state {
            let! randomTile = rand.RandomItem model.Map
            let! () = State.modify (creaturesLens.update (id))
        }
    ()*)
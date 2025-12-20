module Model

open Rogue.Lib
open ProjectUtils
open RayPlatform
open RandomPure
open Accessor

type EntityID =
    | player = -1

type IBehaviour =
    abstract member Position : Accessor<IBehaviour, IntVec>
    abstract member Token : Accessor<IBehaviour, char>

type Model =
    { _map: Set<IntVec>
    ; _seed: RandomPure.RandomSeed
    ; _entities : Map<EntityID, IBehaviour>
    }
    member this.Map =
        { Get = this._map
        ; Change = fun f -> { this with _map = f this._map }}
    member this.Seed =
        { Get = this._seed
        ; Change = fun f -> { this with _seed = f this._seed }}
    member this.Entities =
        { Get = this._entities
        ; Change = fun f -> { this with _entities = f this._entities }}

//move toward a GUID model where entities are stored in an array and carry their own EntityID, allowing find-and-replace semantics
//GUID also allows functions parameters to be refined to IBehaviour subtypes, as passing identifiers is no longer required for model updates
//add an in-game logging system, allowing notifications for player and developer information

let empty =
    { _entities = Map.empty
    ; _map = Set.empty
    ; _seed = Seed 69
    }

let destroyEntity entityID =
    fun (model: Model) ->
        model
        |> (_.Entities $ Map.itemLens entityID) <-- None
        |> Writer.return_
    |> Msg

let spawnEntity entityID (behaviour: 'behaviour when 'behaviour :> IBehaviour) =
    fun (model: Model) ->
        model
        |> (_.Entities $ Map.itemLens entityID) <-- Some (behaviour :> IBehaviour)
        |> Writer.return_
    |> Msg
    
///Dijkstra/A* (Chebyshev norm heuristic)
let findPath (start: IntVec) (finish: IntVec) (model: Model) =
    let freeTiles = Set model.Map.Get
            
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

        match checkPath [] finish with
        | [] ->
            match
                priorityQueue
                |> Map.toSeq
                (* Technically the Chebyshev norm says that ||(x, 0)|| = ||(x, x)||,
                ** which can lead to unnatural looking pathing even though
                ** it's still technically an optimal path under the Chebyshev norm.
                ** For multiple optimal paths, tie-breaking with the Manhattan distance here
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

        | path -> path
    in aux Map.empty (Map.add start (0, None) Map.empty)

let genNewMap (model: Model) =
    BSP.genRandomMap (BSP.Bounds.t (0, 64, 0, 32)) 4 4
    |> _.RunState(model.Seed.Get)
    |> fun (newSeed, newMap) ->
        model
        |> _.Seed <-- newSeed
        |> _.Map <-- newMap

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
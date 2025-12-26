module Model

open Rogue.Lib
open RayPlatform
open ProjectUtils
open Accessor
open RandomPure

type EntityID =
    | player = -69

type IBehaviour =
    abstract member Name: Accessor<IBehaviour, string>
    abstract member Token: Accessor<IBehaviour, char>
    abstract member Position: Accessor<IBehaviour, IntVec>

type Model =
    { _map: Set<IntVec>
    ; _seed: RandomSeed
    ; _entities: Map<EntityID, IBehaviour>
    ; _logs: List<string>
    ; _zoom: int
    }

    static member Make(?seed, ?map, ?entities, ?logs, ?zoom) =
        { _map = defaultArg map Set.empty
        ; _entities = defaultArg entities Map.empty
        ; _seed = defaultArg seed (Seed 32)
        ; _logs = defaultArg logs []
        ; _zoom = defaultArg zoom 30
        }
    static member Empty = Model.Make()

    member this.Map =
        { Get = this._map
        ; Change = fun f -> { this with _map = f this._map } }
    member this.Entities =
        { Get = this._entities
        ; Change = fun f -> { this with _entities = f this._entities } }
    member this.Seed =
        { Get = this._seed
        ; Change = fun f -> { this with _seed = f this._seed } }
    member this.Logs =
        { Get = this._logs
        ; Change = fun f -> { this with _logs = f this._logs } }
    member this.Zoom =
        { Get = this._zoom
        ; Change = fun f -> { this with _zoom = f this._zoom } }
    member this.Item (entityID: EntityID) =
        this |> (_.Entities $ Map.itemLens entityID)

    member this.NextID = Seq.find (not << this._entities.ContainsKey) (Seq.initInfinite enum<EntityID>)

    static member PutLog log =
        fun (model: Model) ->
            model.Logs <-- log :: model.Logs.Get
        |> Msg

    /// If an entity with this ID already exists, will produce a ModelLog error message instead
    static member SpawnEntity (entity: IBehaviour, ?withID: EntityID) = msgCE {
        let! (model: Model) = Msgs.identity

        let entityID = (defaultArg withID model.NextID)

        match model[entityID].Get with
        | None -> yield model[entityID] <-- Some entity
        | Some _ -> yield! Model.PutLog $"ERROR: there is already an entity with the ID {entityID}"
    }

    static member SpawnEntityOnRandomTile (entity: IBehaviour, ?withID: EntityID) = msgCE {
        let! model: Model = Msgs.identity
        let entityID = defaultArg withID model.NextID

        match
            Set.difference
                model._map
                (model._entities.Values |> Seq.map _.Position.Get |> Set)
            |> RandomPure.randomItem
            |> _.RunState(model._seed)
        with
        | newSeed, newPosition ->
            yield model.Seed <-- newSeed
            yield! Model.SpawnEntity (entity.Position <-- newPosition, entityID)
    }

    static member DestroyEntity (entityID: EntityID) = msgCE {
        let! model: Model = Msgs.identity

        match model[entityID].Get with
        | Some _ ->
            yield model[entityID] <-- None
        | None ->
            yield! Model.PutLog $"DestroyEntity ERROR: there is no entity with the ID {entityID}"
    }

    static member GenNewMap = msgCE {
        let! model: Model = Msgs.identity

        yield
            BSP.genRandomMap (BSP.Bounds.t (0, 64, 0, 32)) 4 4
            |> _.RunState(model.Seed.Get)
            |> fun (newSeed, newMap) ->
                ( model
                |> _.Seed <-- newSeed
                |> _.Map <-- newMap
                )
    }

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
                |> Seq.tryMinBy (function pos1, (dist1, _) -> dist1, IntVec.NormManhattan(finish - pos1))
                // tie-breaking with the Manhattan distance here will choose the most natural looking optimal path
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

let move newPos entityID=
    fun (model: Model) ->
        model[entityID] <-* Option.map (fun gamePiece -> gamePiece.Position <-- newPos)
    |> Msg

let moveToward destination entityID = msgCE {
    let! model: Model = Msgs.identity
    match
        model[entityID].Get
        |> Option.toList
        |> List.collect (fun gp -> findPath gp.Position.Get destination model)
    with
    | [] -> yield model
    | nextPos :: _ -> yield! move nextPos entityID
}
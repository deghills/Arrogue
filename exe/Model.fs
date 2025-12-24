module Model

open Rogue.Lib
open RayPlatform
open ProjectUtils
open Accessor

type EntityID =
    | player = -69

type IBehaviour =
    abstract member Position: Accessor<IBehaviour, IntVec>
    abstract member Token: Accessor<IBehaviour, char>

type Model =
    val private _map: Set<IntVec>
    val private _seed: RandomPure.RandomSeed
    val private _entities: Map<EntityID, IBehaviour>
    val private _logs: List<string>
    private new (?map, ?entities, ?seed, ?logs) =
        { _map = defaultArg map Set.empty
        ; _seed = defaultArg seed (RandomPure.Seed 32)
        ; _entities = defaultArg entities Map.empty
        ; _logs = defaultArg logs []
        }

    member this.Map =
        { Get = this._map
        ; Change = fun f -> Model (f this._map, this._entities, this._seed, this._logs) }
    member this.Entities =
        { Get = this._entities
        ; Change = fun f -> Model (this._map, f this._entities, this._seed, this._logs) }
    member this.Seed =
        { Get = this._seed
        ; Change = fun f -> Model (this._map, this._entities, f this._seed, this._logs) }

    member this.Logs = this._logs

    member this.FindEntity (entityID: EntityID) =
        this._entities.TryFind entityID

    member this.NextID = Seq.find (not << this._entities.ContainsKey) (Seq.initInfinite enum<EntityID>)

    static member PutLog log =
        fun (model: Model) ->
            Model (model.Map.Get, model.Entities.Get, model.Seed.Get, log :: model.Logs), []
        |> Msg

    /// If an entity with this ID already exists, will produce a ModelLog error message instead
    static member SpawnEntity (entity: IBehaviour, ?withID: EntityID) =
        fun (model: Model) ->
            let entityID = defaultArg withID model.NextID

            match model._entities.TryFind entityID with
            | None -> Model (model._map, model._entities.Add(entityID, entity), model._seed), []
            | Some _ ->
                ( model
                , [Model.PutLog $"ERROR: there is already an entity with the ID {entityID}"]
                )

        |> Msg

    static member SpawnEntityOnRandomTile (entity: IBehaviour, ?withID: EntityID) =
        fun (model: Model) ->
            let entityID = defaultArg withID model.NextID
            RandomPure.randomItem
                (Set.difference
                    model._map
                    (model._entities.Values |> Seq.map _.Position.Get |> Set)
                )
            |> _.RunState(model._seed)
            |> function
                newSeed, newPosition ->
                    ( model.Seed <-- newSeed
                    , [Model.SpawnEntity (entity.Position <-- newPosition, entityID)]
                    )
        |> Msg

    static member DestroyEntity (entityID: EntityID) =
        fun (model: Model) ->
            match model._entities.TryFind entityID with
            | None ->
                (model
                , [Model.PutLog $"DestroyEntity ERROR: there is entity with the ID {entityID}"]
                )
            | Some _ ->
                Model (model._map, model._entities.Remove entityID, model._seed), []
        |> Msg

    static member GenNewMap =
        fun (model: Model) ->
            BSP.genRandomMap (BSP.Bounds.t (0, 64, 0, 32)) 4 4
            |> _.RunState(model.Seed.Get)
            |> fun (newSeed, newMap) ->
                ( model
                |> _.Seed <-- newSeed
                |> _.Map <-- newMap
                ), []
        |> Msg

    static member Empty = Model()
    
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
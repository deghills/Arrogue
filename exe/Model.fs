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
    private new (map, entitites, seed) = { _map = map; _seed = seed; _entities = entitites }

    member this.Map =
        { Get = this._map
        ; Change = fun f -> Model (f this._map, this._entities, this._seed) }
    member this.Entities =
        { Get = this._entities
        ; Change = fun f -> Model (this._map, f this._entities, this._seed) }
    member this.Seed =
        { Get = this._seed
        ; Change = fun f -> Model (this._map, this._entities, f this._seed) }

    member this.FindEntity (entityID: EntityID) =
        this._entities.TryFind entityID

    member this.NextID = Seq.find (not << this._entities.ContainsKey) (Seq.initInfinite enum<EntityID>)

    /// If an entity with this ID already exists, will produce a ModelLog error message instead
    static member SpawnEntity (entity: IBehaviour, ?withID: EntityID) =
        fun (model: Model) ->
            let entityID = defaultArg withID model.NextID

            match model._entities.TryFind entityID with
            | None -> Model (model._map, model._entities.Add(entityID, entity), model._seed)
            | Some _ -> failwith "make this log the error when you make the logging system"
            |> Writer.return_
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
                    Writer.writer {
                        do! Model.SpawnEntity (entity.Position <-- newPosition, entityID) |> Writer.write
                        return model.Seed <-- newSeed
                    }
        |> Msg

    static member DestroyEntity (entityID: EntityID) =
        fun (model: Model) ->
            match model._entities.TryFind entityID with
            | None -> failwith "make this log the error when you make the logging system"
            | Some _ -> Model (model._map, model._entities.Remove entityID, model._seed)
            |> Writer.return_
        |> Msg

    static member GenNewMap =
        fun (model: Model) ->
            BSP.genRandomMap (BSP.Bounds.t (0, 64, 0, 32)) 4 4
            |> _.RunState(model.Seed.Get)
            |> fun (newSeed, newMap) ->
                model
                |> _.Seed <-- newSeed
                |> _.Map <-- newMap
            |> Writer.return_
        |> Msg

    static member Empty = Model(Set.empty, Map.empty, RandomPure.Seed 69)

//move toward a GUID model where entities are stored in an array and carry their own EntityID, allowing find-and-replace semantics
//GUID also allows functions parameters to be refined to IBehaviour subtypes, as passing identifiers is no longer required for model updates
//add an in-game logging system, allowing notifications for player and developer information
    
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
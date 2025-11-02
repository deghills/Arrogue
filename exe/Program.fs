open ZeroElectric.Vinculum
open Rogue.Lib
open ProjectUtils
open RayPlatform

let cellSize = 50

let drawChar (Vec (x, y)) (chr: char) =
    Raylib.DrawText (string chr, x * cellSize, y * cellSize, cellSize, Raylib.RAYWHITE)

type CreatureID =
    | player = -69

type CreatureSheet =
    { Health: int
    ; Strength: int
    }

type Creature =
    { Pos: IntVec
    ; Token: char
    ; Stats: CreatureSheet
    }
    static member View { Pos = p; Token = token } = p, token
    static member Attack (c1: Creature) (c2: Creature) : Creature option =
        let (|Dead|_|) c = c.Stats.Health < 1
        match { c2 with Stats.Health = c2.Stats.Health - c1.Stats.Strength } with
        | Dead -> None
        | alive -> Some alive
        //|> function dead when dead.Stats.Health < 1 -> None | alive -> Some alive

type State =
    { Creatures: Map<CreatureID, Creature>
    ; Walls: Set<IntVec>
    }

    member this.TransformCreatures f = { this with Creatures = f this.Creatures }

    ///Dijkstra/A* (Euclidean norm heuristic)
    member this.FindPath (start: IntVec) (finish: IntVec) =
        let occupiedTiles =
            Map.toSeq this.Creatures
            |> Seq.map (snd >> _.Pos)
            |> Set
            |> Set.union this.Walls
            |> Set.remove start
            |> Set.remove finish
            
        let getNeighbours (p: IntVec) =
            Set.difference
                (Set.empty
                    .Add(p + IntVec.Up)
                    .Add(p + IntVec.Up + IntVec.Right)
                    .Add(p + IntVec.Up + IntVec.Left)
                    .Add(p + IntVec.Left)
                    .Add(p + IntVec.Right)
                    .Add(p + IntVec.Down)
                    .Add(p + IntVec.Down + IntVec.Right)
                    .Add(p + IntVec.Down + IntVec.Left)
                )
                occupiedTiles

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
                    ** will choose a more natural looking one that tries to axis-align with the target *)
                    |> Seq.sortBy (function pos1, (dist1, _) -> dist1, IntVec.NormManhattan(finish - pos1))
                    |> Seq.tryHead
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

type Msg =
    | GenericAction of CreatureID * IntVec
    | AttackCreature of CreatureID * CreatureID
    | MoveCreatureTo of CreatureID * IntVec
    | MoveCreatureToward of CreatureID * IntVec
    | EnvironmentTurn
    | GameOver

let init =
    { Creatures =
        Map
            [ CreatureID.player, { Pos = IntVec.Vec (20, 20); Token = '@'; Stats = { Health = 100; Strength = 10 } }
            ; enum<CreatureID> 0, { Pos = IntVec.Vec (15, 15); Token = 'g'; Stats = { Health = 100; Strength = 11 } } ]
    ; Walls =
        Set
            [ for j in 0..20 do if j <> 13 then yield Vec (17, j) ]
    }
let view state =
    if state.Creatures.ContainsKey CreatureID.player |> not then
        Raylib.DrawText
            ( "the player has died!\npress any key to close window"
            , 0
            , 0
            , 20
            , Raylib.RAYWHITE
            )
    else
        state.Creatures
        |> Map.toSeq
        |> Seq.fold
            (fun acc (_, c) -> Map.add c.Pos c.Token acc)
            Map.empty
        |> fun charmap ->
            Seq.fold
                (fun acc wallPos -> Map.add wallPos '=' acc)
                charmap
                state.Walls
        |> Map.iter drawChar

let update state msg : (State*Msg list) =
    let appendMsgs msgs x = (x, msgs)
    let gameover = state, [GameOver]
    let pass = state, []
    match msg with
    | GameOver -> Raylib.CloseWindow(); exit 0

    | GenericAction (creatureID, targetPos) ->
        state.Creatures.TryFind creatureID
        |> Option.toResult pass
        |> Result.bind
            (fun creature ->
                let targetIsInRange = IntVec.NormChebyshev (targetPos - creature.Pos) <= 1
                match Map.tryFindKey (fun _ c -> c.Pos = targetPos) state.Creatures with
                | Some targetID when targetIsInRange ->
                    Ok (state, [AttackCreature (creatureID, targetID)])
                | _ ->
                    Error (state, [MoveCreatureToward (creatureID, targetPos)])
            )
        |> function Ok result | Error result -> result

    | MoveCreatureTo (creatureID, newPos) ->
        Map.change
            creatureID
            (fun creature ->
                let spaceIsOccupied =
                    Map.exists (konst (_.Pos >> (=) newPos)) state.Creatures
                    //||
                    //Set.contains newPos state.Walls
                if spaceIsOccupied then
                    creature
                else
                    { creature with Pos = newPos }
            |> Option.map
            )
        |> state.TransformCreatures
        |> appendMsgs []

    | MoveCreatureToward (creatureID, destination) ->
        match Map.tryFind creatureID state.Creatures with
        | None ->
            pass
        | Some creature ->
            match state.FindPath creature.Pos destination with
            | nextPos :: _ -> state, [MoveCreatureTo (creatureID, nextPos)]
            | [] -> state, []

    | AttackCreature (attackerID, targetID) ->
        Map.tryFind attackerID state.Creatures
        |> Option.map
            ( Creature.Attack
            >> Option.bind
            >> Map.change targetID
            >> state.TransformCreatures
            >> appendMsgs []
            )
        |> Option.defaultValue
            pass

    | EnvironmentTurn ->
        match Map.tryFind CreatureID.player state.Creatures with
        | None ->
            gameover

        | Some player ->
            ( state
            , List.choose
                (function
                | CreatureID.player, _ ->
                    None
                | npcID, _ ->
                    GenericAction (npcID, player.Pos) |> Some
                )
                (Map.toList state.Creatures)
            )

let subscriptions (tick: RayPlatform.TickInfo) (state: State) =
    match state.Creatures.TryFind CreatureID.player with
    | Some player ->
        [ if tick[KeyboardKey.KEY_W].IsPressed then
            yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (0, -1))
            yield EnvironmentTurn
        ; if tick[KeyboardKey.KEY_A].IsPressed then
            yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (-1, 0))
            yield EnvironmentTurn
        ; if tick[KeyboardKey.KEY_S].IsPressed then
            yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (0, 1))
            yield EnvironmentTurn
        ; if tick[KeyboardKey.KEY_D].IsPressed then
            yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (1, 0))
            yield EnvironmentTurn
        ; if tick[KeyboardKey.KEY_SPACE].IsPressed then
            yield EnvironmentTurn
        ]
    | None ->
        [ if tick.PressedKeys.Length > 0 then yield GameOver ]

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    update
    subscriptions
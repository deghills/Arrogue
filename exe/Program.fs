open System
open ZeroElectric.Vinculum
open ProjectUtils

let cellSize = 50

let drawChar (IntVec.Vec (x, y)) (chr: char) =
    Raylib.DrawText (string chr, x * cellSize, y * cellSize, cellSize, Raylib.GREEN)

Raylib.InitWindow (1920, 1080, "i fucking hate my life")

type CreatureID =
    | player = -3

type CreatureSheet =
    { Health: int
    ; Strength: int
    }
  
type State =
    { Creatures: Map<CreatureID, Creature>
    }
    member this.TransformCreatures f = { this with Creatures = f this.Creatures}

and Creature =
    { Pos: IntVec.t
    ; Token: char
    ; Stats: CreatureSheet
    }
    static member View { Pos = p; Token = token } = p, token
    static member Attack (c1: Creature) (c2: Creature) : Creature option =
        { c2 with Stats.Health = c2.Stats.Health - c1.Stats.Strength }
        |> function dead when dead.Stats.Health < 1 -> None | alive -> Some alive

type PlayerAction =
    | TryTile of IntVec.t

type Msg =
    | PlayerTurn of PlayerAction
    | AttackCreature of CreatureID*CreatureID
    | MoveCreatureToPosition of CreatureID * IntVec.t
    | EnvironmentTurn
    | GameOver

let init =
    { Creatures =
        Map
            [ CreatureID.player, { Pos = IntVec.Vec (10, 10); Token = '@'; Stats = { Health = 100; Strength = 10 } }
            ; enum<CreatureID> 0, { Pos = IntVec.Vec (20, 20); Token = 'g'; Stats = { Health = 100; Strength = 11 } } ]
    }
let view =
    _.Creatures
    >> Map.fold
        (fun acc _ c -> (Creature.View c ||> Map.add) acc)
        Map.empty
    >> Map.iter drawChar

let update state msg : (State*Msg list) =
    let appendMsgs msgs x = (x, msgs)
    let gameover = state, [GameOver]
    let pass = state, []
    match msg with
    | GameOver -> Raylib.CloseWindow(); exit 0

    | PlayerTurn (TryTile translation) ->
        Map.tryFind CreatureID.player state.Creatures
        |> Option.toResult gameover
        |> Result.map
            (fun { Pos = playerPos } -> playerPos + translation)
        |> Result.bind
            (fun nextPlayerPos ->
                match
                    Seq.tryFind
                        (fun (colliderID, { Pos = colliderPos }) ->
                            colliderPos = nextPlayerPos
                            && colliderID <> CreatureID.player
                        )
                        (Map.toSeq state.Creatures)
                with
                | Some (colliderID, _) ->
                    ( state
                    , [AttackCreature (CreatureID.player, colliderID)]
                    ) |> Error
                | None ->
                    ( state
                    , [MoveCreatureToPosition (CreatureID.player, nextPlayerPos)]
                    ) |> Ok
            )
        |> function Ok result | Error result -> result

    | MoveCreatureToPosition (creatureID, newPos) ->
        Map.change
            creatureID
            (fun player ->
                let spaceIsOccupied = Map.exists (konst (_.Pos >> (=) newPos)) state.Creatures
                if spaceIsOccupied then
                    player
                else
                    { player with Pos = newPos }
            |> Option.map
            )
        |> state.TransformCreatures
        |> appendMsgs []

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
        Map.tryFind CreatureID.player state.Creatures
        |> Option.toResult gameover
        |> Result.map
            (fun player ->
                List.collect
                    (function
                    | CreatureID.player, _ ->
                        []
                    | npcID, npc ->
                        // maybe ought to make a "MoveCreatureToward" Msg, so I can delegate instead of doing pathfinding in this branch
                        let pathToPlayer: IntVec.t list = failwith "implement dijkstra/A* algorithm"
                        match pathToPlayer with
                        | [] -> [AttackCreature (npcID, CreatureID.player)]
                        | nextSpot :: _ -> [MoveCreatureToPosition (npcID, nextSpot)]
                    )
                    (Map.toList state.Creatures)
            )
        |> Result.map appendMsgs
        |> Result.map ((|>) state)
        |> function Ok result | Error result -> result

let subscriptions (tick: RayPlatform.TickInfo) (s: State) =
    [ if tick[KeyboardKey.KEY_W].IsPressed then yield IntVec.Vec (0, -1) |> PlayerAction.TryTile |> PlayerTurn
    ; if tick[KeyboardKey.KEY_A].IsPressed then yield IntVec.Vec (-1, 0) |> PlayerAction.TryTile |> PlayerTurn
    ; if tick[KeyboardKey.KEY_S].IsPressed then yield IntVec.Vec (0, 1) |> PlayerAction.TryTile |> PlayerTurn
    ; if tick[KeyboardKey.KEY_D].IsPressed then yield IntVec.Vec (1, 0) |> PlayerAction.TryTile |> PlayerTurn
    ; if tick[KeyboardKey.KEY_SPACE].IsPressed then yield EnvironmentTurn
    ]

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    update
    subscriptions
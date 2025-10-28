open ZeroElectric.Vinculum
open Rogue.Lib
open ProjectUtils
open RayPlatform

let cellSize = 50

let drawChar (Vec (x, y)) (chr: char) =
    Raylib.DrawText (string chr, x * cellSize, y * cellSize, cellSize, Raylib.GREEN)

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
        { c2 with Stats.Health = c2.Stats.Health - c1.Stats.Strength }
        |> function dead when dead.Stats.Health < 1 -> None | alive -> Some alive

type Tile =
    | Empty
    | Creature of Creature
    | Wall of IntVec

type TileMap =
    val tiles: array<array<Tile>>
    member this.TrySpawn (creature: Creature) (pos: IntVec) =
        ()

    new (initializer: int -> int -> Tile, width, height) =
        { tiles =
            Array.init
                width
                (fun col ->
                    Array.init
                        height
                        (fun row -> initializer col row)
                )
        }

type State =
    { Creatures: Map<CreatureID, Creature>
    ; Walls: Set<IntVec>
    }
    member this.TransformCreatures f = { this with Creatures = f this.Creatures }
    member this.FindPath (start: IntVec) (finish: IntVec) =
        let width =
            Map.toSeq this.Creatures
            |> Seq.map (snd >> _.Pos >> _.X)
            |> Seq.append (this.Walls |> Seq.map _.X)
            |> Seq.max

        let height =
            Map.toSeq this.Creatures
            |> Seq.map (snd >> _.Pos >> _.Y)
            |> Seq.max

        //i gotta figure out how to make an "infinite" plane work with dijkstra's algorithm
        //lazy data structures are hard to think about

        let lattice =
            this.Walls
            |> Set.fold
                (fun (graph: Graph.Simple<IntVec>) vertexToRemove -> graph.RemoveVertex vertexToRemove)
                (Graph.Simple<IntVec>.Lattice width height)

        Graph.Simple.Dijkstra start finish lattice

type PlayerAction =
    | TryTile of IntVec

type Msg =
    | PlayerTurn of PlayerAction
    | AttackCreature of CreatureID*CreatureID
    | MoveCreatureTo of CreatureID * IntVec
    | MoveCreatureToward of CreatureID * IntVec
    | EnvironmentTurn
    | GameOver

let init =
    { Creatures =
        Map
            [ CreatureID.player, { Pos = IntVec.Vec (20, 20); Token = '@'; Stats = { Health = 100; Strength = 10 } }
            ; enum<CreatureID> 0, { Pos = IntVec.Vec (15, 15); Token = 'g'; Stats = { Health = 100; Strength = 11 } } ]
    ; Walls = Set []
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
                    , [AttackCreature (CreatureID.player, colliderID); EnvironmentTurn]
                    ) |> Error
                | None ->
                    ( state
                    , [MoveCreatureTo (CreatureID.player, nextPlayerPos); EnvironmentTurn]
                    ) |> Ok
            )
        |> function Ok result | Error result -> result

    | MoveCreatureTo (creatureID, newPos) ->
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

    | MoveCreatureToward (creatureID, destination) ->
        match Map.tryFind creatureID state.Creatures with
        | None -> pass
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
            state,
            List.choose
                (function
                | CreatureID.player, _ ->
                    None
                | npcID, _ ->
                    MoveCreatureToward (npcID, player.Pos) |> Some
                )
                (Map.toList state.Creatures)

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
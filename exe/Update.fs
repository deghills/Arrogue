module Update

open Rogue.Lib
open RayPlatform.Msg
open ProjectUtils
open Creature
open Model

type Msg =
    | GenericAction of CreatureID * IntVec
    | AttackCreature of CreatureID * CreatureID
    | MoveCreatureTo of CreatureID * IntVec
    | MoveCreatureToward of CreatureID * IntVec
    | EnvironmentTurn

    interface RayPlatform.Msg.IMsg<Model> with
        member this.UpdateModel state =
            let appendMsgs msgs x = (x, msgs)
            let pass = state, []

            match this with
            | GenericAction (creatureID, targetPos) ->
                state.Creatures.TryFind creatureID
                |> Option.toResult pass
                |> Result.bind
                    (fun creature ->
                        let targetIsInRange = IntVec.NormChebyshev (targetPos - creature.Pos) <= 1
                        match Map.tryFindKey (fun _ c -> c.Pos = targetPos) state.Creatures with
                        | Some targetID when targetIsInRange ->
                            Ok (state, [AttackCreature (creatureID, targetID) :> IMsg<Model>])
                        | _ ->
                            Error (state, [MoveCreatureToward (creatureID, targetPos) :> IMsg<Model>])
                    )
                |> function Ok result | Error result -> result

            | MoveCreatureTo (creatureID, newPos) ->
                let spaceIsOccupied =
                    Map.exists (konst (_.Pos >> (=) newPos)) state.Creatures
                    ||
                    not (Set.contains newPos state.Map)

                if spaceIsOccupied then
                    pass
                else
                    Map.change
                        creatureID
                        (Option.map (fun creature -> { creature with Pos = newPos } ))
                    |> state.TransformCreatures
                    |> appendMsgs [ if creatureID = CreatureID.player then yield EnvironmentTurn ]

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
                    >> appendMsgs [ if attackerID = CreatureID.player then yield EnvironmentTurn :> IMsg<Model>]
                    )
                |> Option.defaultValue
                    pass

            | EnvironmentTurn ->
                match Map.tryFind CreatureID.player state.Creatures with
                | None -> state, [ Quit ]

                | Some player ->
                    ( state
                    , List.choose
                        (function
                        | CreatureID.player, _ ->
                            None
                        | npcID, _ ->
                            GenericAction (npcID, player.Pos)
                            :> IMsg<Model>
                            |> Some
                        )
                        (Map.toList state.Creatures)
                    )
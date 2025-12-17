module Update

open Rogue.Lib
open RayPlatform.Msg
open ProjectUtils
open Creature
open Model

type Msg =
    | GenericAction of EntityID * IntVec
    | AttackCreature of EntityID * EntityID
    | MoveCreatureTo of EntityID * IntVec
    | MoveCreatureToward of EntityID * IntVec
    | EnvironmentTurn

    interface RayPlatform.Msg.IMsg<Model> with
        member this.Update state =
            let appendMsgs msgs x = (x, msgs)
            let pass = state, []

            match this with
            | GenericAction (creatureID, targetPos) ->
                Map.tryFind creatureID state.Tiles
                |> Option.toResult pass
                |> Result.bind
                    (fun creature ->
                        let targetIsInRange = IntVec.NormChebyshev (targetPos - creature.Pos) <= 1
                        match Map.tryFindKey (fun _ c -> c.Pos = targetPos) state.Tiles with
                        | Some targetID when targetIsInRange ->
                            Ok (state, [AttackCreature (creatureID, targetID) :> IMsg<Model>])
                        | _ ->
                            Error (state, [MoveCreatureToward (creatureID, targetPos) :> IMsg<Model>])
                    )
                |> function Ok result | Error result -> result

            | MoveCreatureTo (creatureID, newPos) ->
                let spaceIsOccupied =
                    Map.exists (konst (_.Pos >> (=) newPos)) state.Tiles
                    ||
                    not (Set.contains newPos state.Map)

                if spaceIsOccupied then
                    pass
                else
                    Map.change
                        creatureID
                        (Option.map (fun creature -> { creature with Pos = newPos } ))
                    |> flip Model.tilesLens.update state
                    |> appendMsgs [ if creatureID = EntityID.player then yield EnvironmentTurn ]

            | MoveCreatureToward (creatureID, destination) ->
                match Map.tryFind creatureID state.Tiles with
                | None ->
                    pass
                | Some creature ->
                    match Model.findPath creature.Pos destination state with
                    | nextPos :: _ -> state, [MoveCreatureTo (creatureID, nextPos)]
                    | [] -> state, []

            | AttackCreature (attackerID, targetID) ->
                Map.tryFind attackerID state.Creatures
                |> Option.map
                    ( Creature.attack
                    >> Option.bind
                    >> Map.change targetID
                    >> flip Model.creaturesLens.update state
                    >> appendMsgs [ if attackerID = EntityID.player then yield EnvironmentTurn :> IMsg<Model>]
                    )
                |> Option.defaultValue
                    pass

            | EnvironmentTurn ->
                match Map.tryFind EntityID.player state.Tiles with
                | None -> state, [ Quit ]

                | Some player ->
                    ( state
                    , List.choose
                        (function
                        | EntityID.player, _ ->
                            None
                        | npcID, _ ->
                            GenericAction (npcID, player.Pos)
                            :> IMsg<Model>
                            |> Some
                        )
                        (Map.toList state.Creatures)
                    )
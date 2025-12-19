module Update

open Rogue.Lib
open RayPlatform
open PlatformMsgs
open ProjectUtils
open Model

// i think I will dissolve the Msg type and just have them all be separate IMsg values.
// only thing I need to be careful of is if losing mututal recursion when doing this matters
// at the very least, maybe this will cut out some of the boilerplate and indentation depth
    
module GamePieces =
    let move entity newPos =
        Model.gamePiecesL
        $ Map.itemLens entity
        <-* Option.map (fun gp -> { gp with Pos = newPos })
        >> Writer.return_
        |> Msg

    let moveToward entity destination =
        fun state ->
            state
            .> (Model.gamePiecesL $ Map.itemLens entity)
            |> Option.toList
            |> List.collect (fun gp -> Model.findPath gp.Pos destination state)
            |> function
                | [] -> Writer.return_ state
                
                | nextPos :: _ ->
                    Writer.writer {
                        do! Writer.write (move entity nextPos)
                        return state
                    }
        |> Msg

module Creature =
    let attackCreature attackerID defenderID =
        fun state ->
            Option.option {
                let! attacker = state .> (Model.creaturesL $ Map.itemLens attackerID)
                return
                    ( Model.creaturesL
                    $ Map.itemLens defenderID
                    <-* Option.bind (Creature.attack attacker)
                    ) state
            }
            |> Option.defaultValue state
            |> Writer.return_
        |> Msg

    let creatureAI creatureID =
        fun state ->
            if creatureID = EntityID.player
            then Writer.return_ state
            else

            match
                state .> (Model.gamePiecesL $ Map.itemLens creatureID),
                state .> (Model.gamePiecesL $ Map.itemLens EntityID.player)
            with
            | Some { Pos = creaturePos }, Some { Pos = playerPos } ->
                Writer.writer {
                    do! if IntVec.NormChebyshev (playerPos - creaturePos) <= 1
                        then Writer.write (attackCreature creatureID EntityID.player)
                        else Writer.write (GamePieces.moveToward creatureID playerPos)

                    return state
                }
            | _ -> Writer.return_ state
        |> Msg

    let environmentTurn =
        fun (state: Model) ->
            let x =
                Writer.writer {
                    for creatureID in Map.keys state.Creatures do
                        do! Writer.write (creatureAI creatureID)

                    return state
                }
            x
        |> Msg

    let playerGenericAction selectedTile =
        fun state ->
            match state .> (Model.gamePiecesL $ Map.itemLens EntityID.player) with
            | None -> Writer.return_ state

            | Some { Pos = playerPos } ->
                let targetIsInRange = IntVec.NormChebyshev (selectedTile - playerPos) <= 1
                Writer.writer {
                    match Map.tryFindKey (fun _ c -> c.Pos = selectedTile) state.Tiles with
                    | Some targetID when targetIsInRange ->
                        do! Writer.write (attackCreature EntityID.player targetID)
                        do! Writer.write environmentTurn
                    | _ ->
                        match Model.findPath playerPos selectedTile state with
                        | [] -> ()
                        | nextPos :: _ ->
                            do! Writer.write (GamePieces.move EntityID.player nextPos)
                            do! Writer.write environmentTurn

                    return state
                }
        |> Msg

(*type Msg =
    | GenericAction of EntityID * IntVec
    | AttackCreature of EntityID * EntityID
    | MoveToTile of EntityID * IntVec
    | MoveTowardTile of EntityID * IntVec
    | EnvironmentTurn

    interface IMsg<Model> with
        member this.Update state =
            let appendMsgs msgs x = (x, msgs)
            let pass = state, []

            match this with
            | 

            | *)
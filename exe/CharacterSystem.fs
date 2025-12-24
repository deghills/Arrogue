module Update

open Rogue.Lib
open RayPlatform
open ProjectUtils
open Model
open Accessor

type ICharacterSheet = interface
    inherit IBehaviour
    //abstract member Name : Accessor<ICharacterSheet, string>
    abstract member Strength : Accessor<ICharacterSheet, int>
    abstract member Health : Accessor<ICharacterSheet, int>
    static member Qualify: IBehaviour -> option<ICharacterSheet> = function
        :? ICharacterSheet as chrSht -> Some chrSht | _ -> None
end

type Creature (strength, health, token, position) = class
    interface ICharacterSheet with
        member _.Strength =
            { Accessor.Get = strength; Accessor.Change = fun f -> Creature (f strength, health, token, position) }
        member _.Health =
            { Accessor.Get = health; Accessor.Change = fun f -> Creature (strength, f health, token, position) }
        member _.Token =
            { Accessor.Get = token; Accessor.Change = fun f -> Creature (strength, health, f token, position) }
        member _.Position =
            { Accessor.Get = position; Accessor.Change = fun f -> Creature (strength, health, token, f position) }

    new() = Creature (11, 100, 'g', IntVec.Zero)
end

let hurtCreature damage creatureID =
    fun (model: Model) ->
        Writer.writer {
            let targetAccessor = model |> (_.Entities $ Map.itemLens creatureID)
            match targetAccessor.Get with
            | Some (:? ICharacterSheet as target) ->
                let target' = target.Health <-* (fun health -> health - damage)
                do! Model.PutLog $"{creatureID} has taken {damage} damage" |> Writer.write

                if target'.Health.Get <= 0 then
                    do! Model.PutLog $"{creatureID} has died" |> Writer.write
                    return targetAccessor <-- None

                else
                    return targetAccessor <-- Some target'

            | _ ->
                do! Model.PutLog $"ERROR: there is no creature with the ID: {creatureID}" |> Writer.write
                return model
        } |> Writer.unwrap
    |> Msg

let attackCreature attackerID defenderID =
    fun (model: Model) ->
        match model.Entities.Get.TryFind(attackerID) |> Option.bind ICharacterSheet.Qualify with
        | Some attacker -> model, [hurtCreature (attacker.Strength.Get) defenderID]
        | None -> model, []
    |> Msg

let creatureAI creatureID =
    fun (model: Model) ->
        if creatureID = EntityID.player
        then model, []
        else

        match
            model .> (_.Entities $ Map.itemLens creatureID),
            model .> (_.Entities $ Map.itemLens EntityID.player)
        with
        | Some creature, Some player->
            ( model
            , if IntVec.NormChebyshev (player.Position.Get - creature.Position.Get) <= 1
                then [attackCreature creatureID EntityID.player]
                else [moveToward creatureID player.Position.Get]
            )
        
        | _ -> model, []
    |> Msg

let environmentTurn =
    fun (model: Model) ->
        (model
        , [for creatureID in Map.keys model.Entities.Get -> creatureAI creatureID]
        )
    |> Msg

let playerGenericAction selectedTile =
    fun (model: Model) ->
        match model .> (_.Entities $ Map.itemLens EntityID.player) with
        | None -> model, []

        | Some player ->
            let targetIsInRange = IntVec.NormChebyshev (selectedTile - player.Position.Get) <= 1
            Writer.writer {
                match model.Entities.Get |> Map.tryFindKey (fun _ c -> c.Position.Get = selectedTile) with
                | Some targetID when targetIsInRange ->
                    do! Writer.write (attackCreature EntityID.player targetID)
                    do! Writer.write environmentTurn
                | _ ->
                    match Model.findPath player.Position.Get selectedTile model with
                    | [] -> ()
                    | nextPos :: _ ->
                        do! Writer.write (move EntityID.player nextPos)
                        do! Writer.write environmentTurn

                return model
            } |> Writer.unwrap
    |> Msg
module Update

open Rogue.Lib
open RayPlatform
open ProjectUtils
open Model
open Accessor

let move entityID newPos =
    fun (model: Model) ->
        model
        |> (_.Entities $ Map.itemLens entityID )
        <-*Option.map (fun gp -> gp.Position <-- newPos)
        |> Writer.return_
    |> Msg

let moveToward entityID destination =
    fun (model: Model) ->
        model
        |> (_.Entities $ Map.itemLens entityID)
        |> _.Get
        |> Option.toList
        |> List.collect (fun gp -> Model.findPath gp.Position.Get destination model)
        |> function
            | [] -> Writer.return_ model
            | nextPos :: _ ->
                Writer.writer {
                    do! Writer.write (move entityID nextPos)
                    return model
                }
    |> Msg

type ICharacterSheet = interface
    inherit IBehaviour
    abstract member Health : Accessor<ICharacterSheet, int>
    abstract member Strength : Accessor<ICharacterSheet, int>
    static member Qualify: obj -> option<ICharacterSheet> = function
        :? ICharacterSheet as chrSht -> Some chrSht | _ -> None
end

let hurtCreature damage creatureID =
    fun (model: Model) ->
        model
        |> (_.Entities $ Map.itemLens creatureID)
        <-* Option.bind (function
            | :? ICharacterSheet as target ->
                let damagedTarget = target.Health <-* (fun health -> health - damage)
                if damagedTarget.Health.Get <= 0 then None else Some damagedTarget
            | x -> Some x
            )
        |> Writer.return_
    |> Msg

let attackCreature attackerID defenderID =
    fun (model: Model) ->
        Option.option {
            let! attacker = model.Entities.Get.TryFind(attackerID) |> Option.bind (function :? ICharacterSheet as chrSht -> Some chrSht | _ -> None)

            return Writer.writer {
                do! hurtCreature (attacker.Strength.Get) defenderID |> Writer.write
                return model
            }
        } |> Option.defaultValue (Writer.return_ model)
    |> Msg

let creatureAI creatureID =
    fun (model: Model) ->
        if creatureID = EntityID.player
        then Writer.return_ model
        else

        match
            model .> (_.Entities $ Map.itemLens creatureID),
            model .> (_.Entities $ Map.itemLens EntityID.player)
        with
        | Some creature, Some player->
            Writer.writer {
                do! if IntVec.NormChebyshev (player.Position.Get - creature.Position.Get) <= 1
                    then Writer.write (attackCreature creatureID EntityID.player)
                    else Writer.write (moveToward creatureID player.Position.Get)

                return model
            }
        | _ -> Writer.return_ model
    |> Msg

let environmentTurn =
    fun (model: Model) ->
        Writer.writer {
            for creatureID in Map.keys model.Entities.Get do
                do! Writer.write (creatureAI creatureID)

            return model
        }
    |> Msg

let playerGenericAction selectedTile =
    fun (model: Model) ->
        match model .> (_.Entities $ Map.itemLens EntityID.player) with
        | None -> Writer.return_ model

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
            }
    |> Msg


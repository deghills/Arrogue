module CharacterSystem

open Rogue.Lib
open RayPlatform
open ProjectUtils
open Accessor
open Model
open ItemSystem

type ICharacterSheet = interface
    inherit IContainer
    //abstract member Name : Accessor<ICharacterSheet, string>
    abstract member Strength : Accessor<ICharacterSheet, int>
    abstract member Health : Accessor<ICharacterSheet, int>
    static member Qualify: IBehaviour -> option<ICharacterSheet> = function
        :? ICharacterSheet as chrSht -> Some chrSht | _ -> None
end

type Creature =
    { _name: string; _position: IntVec; _token: char; _strength: int; _health: int; _contents: List<IItem> }

    static member Make(name, token, strength, health, ?position, ?contents) =
        { _name = name
        ; _position = defaultArg position IntVec.Zero
        ; _token = token
        ; _strength = strength
        ; _health = health
        ; _contents = defaultArg contents []
        }

    interface ICharacterSheet with
        member this.Name = { Get = this._name; Change = fun f -> { this with _name = f this._name } }
        member this.Position = { Get = this._position; Change = fun f -> { this with _position = f this._position } }
        member this.Token = { Get = this._token; Change = fun f -> { this with _token = f this._token } }
        member this.Strength = { Get = this._strength; Change = fun f -> { this with _strength = f this._strength } }
        member this.Health = { Get = this._health; Change = fun f -> { this with _health = f this._health } }
        member this.Contents = { Get = this._contents; Change = fun f -> { this with _contents = f this._contents } }

        member this.GetEnumerator (): System.Collections.Generic.IEnumerator<IItem> =
            (this._contents :> seq<IItem>).GetEnumerator()

        member this.GetEnumerator (): System.Collections.IEnumerator =
            (this :> seq<IItem>).GetEnumerator()

let hurtCreature damage creatureID =
    fun (model: Model) ->
        Writer.writer {
            let targetAccessor = model |> (_.Entities $ Map.itemLens creatureID)
            match targetAccessor.Get with
            | Some (:? ICharacterSheet as target) ->
                let target' = target.Health <-* (fun health -> health - damage)
                do! Model.PutLog $"{target.Name.Get} has taken {damage} damage" |> Writer.write

                if target'.Health.Get <= 0 then
                    do! Model.PutLog $"{target.Name.Get} has died" |> Writer.write
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
        match
            model .> (_.Entities $ Map.itemLens attackerID),
            model .> (_.Entities $ Map.itemLens defenderID)
        with
        | Some (:? ICharacterSheet as attacker), Some (:? ICharacterSheet) ->
            model, [hurtCreature (attacker.Strength.Get) defenderID]

        | _ -> model, [Model.PutLog "ERROR: attackCreature invalid input"]
    |> Msg

let creatureAI creatureID =
    fun (model: Model) ->
        match
            model .> (_.Entities $ Map.itemLens creatureID),
            model .> (_.Entities $ Map.itemLens EntityID.player)
        with
        | Some (:? ICharacterSheet as creature), Some (:? ICharacterSheet as player) when creatureID <> EntityID.player ->
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
            Option.option {
                let! targetID =
                    model .> _.Entities |> Map.tryFindKey (fun _ c -> c.Position.Get = selectedTile)

                let targetLens = model |> (_.Entities $ Map.itemLens targetID)

                return
                    match targetLens.Get with
                    | Some (:? ICharacterSheet) when targetIsInRange ->
                        [ attackCreature EntityID.player targetID
                        ; environmentTurn
                        ]

                    | Some (:? IItem) when targetIsInRange ->
                        [ ItemSystem.pickUpItem targetID EntityID.player
                        ; Model.moveToward EntityID.player selectedTile
                        ; environmentTurn
                        ]

                    | _ ->
                        [ Model.PutLog "You cannot do that"
                        ]

            } |> function Some msgs -> model, msgs | None -> model, [Model.moveToward EntityID.player selectedTile; environmentTurn]
            
            (*Writer.writer {
                match model .> _.Entities |> Map.values |> Seq.tryFind (fun c -> c.Position.Get = selectedTile) with
                | Some targetID when targetIsInRange ->
                    do! Writer.write (attackCreature EntityID.player targetID)
                    do! Writer.write environmentTurn
                | _ ->
                    match Model.findPath player.Position.Get selectedTile model with
                    | [] -> ()
                    | nextPos :: _ ->
                        do! Writer.write (move EntityID.player nextPos)
                        do! Writer.write environmentTurn

                return model*)
    |> Msg
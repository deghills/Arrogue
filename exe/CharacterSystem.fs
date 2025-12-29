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

let hurtCreature damage creatureID = msgCE {
    let! model: Model = Msgs.identity

    match model[creatureID].Get with
    | Some (:? ICharacterSheet as target) ->
        let target = target.Health <-* (fun health -> health - damage)
        
        let! model = Model.PutLog $"{target.Name.Get} has taken {damage} damage"

        if target.Health.Get <= 0 then
            let! model = Model.PutLog $"{target.Name.Get} has died"
            yield model[creatureID] <-- None

        else yield model[creatureID] <-- Some target

    | _ -> yield! Model.PutLog $"ERROR: there is no creature with the ID: {creatureID}"
}

let attackCreature attackerID defenderID = msgCE {
    let! model: Model = Msgs.identity

    match model[attackerID].Get, model[defenderID].Get with
    | Some (:? ICharacterSheet as attacker), Some (:? ICharacterSheet) ->
        yield! hurtCreature (attacker.Strength.Get) defenderID

    | _ -> yield! Model.PutLog "ERROR: attackCreature invalid input"
}

let creatureAI creatureID = msgCE {
    let! model: Model = Msgs.identity
    match
        model[creatureID].Get, model[EntityID.player].Get
    with
    | Some (:? ICharacterSheet as creature), Some (:? ICharacterSheet as player) when creatureID <> EntityID.player ->
        if IntVec.NormChebyshev (player.Position.Get - creature.Position.Get) <= 1
            then yield! attackCreature creatureID EntityID.player
            else yield! moveToward player.Position.Get creatureID
        
    | _ -> yield! Msgs.identity
}

let environmentTurn = msgCE {
    let! model: Model = Msgs.identity
    for creatureID in model .> _.Entities |> Map.keys do
        yield! creatureAI creatureID
}

let playerGenericAction selectedTile = msgCE {
    let! model: Model = Msgs.identity
    
    yield! Result.result {
        let! player =
            model[EntityID.player].Get
            |> Option.toResult Msgs.identity

        if player.Position.Get = selectedTile then
            do! Model.PutLog $"""{player.Name.Get}: "What was that?" """ |> Error
        
        let! targetID =
            model
            .> _.Entities
            |> Map.tryFindKey (fun _ c -> c.Position.Get = selectedTile)
            |> Option.toResult (msgCE {
                match Model.findPath player.Position.Get selectedTile model with
                | nextTile :: _ -> yield! move nextTile EntityID.player; yield! environmentTurn
                | [] -> yield! Model.PutLog "you cannot do that"
            })

        let targetIsInRange = IntVec.NormChebyshev (selectedTile - player.Position.Get) <= 1

        return!
            match model[targetID].Get with
            | Some (:? ICharacterSheet) when targetIsInRange ->
                msgCE {
                    yield! attackCreature EntityID.player targetID
                    yield! environmentTurn
                } |> Ok

            | Some (:? IItem) when targetIsInRange ->
                msgCE {
                    yield! ItemSystem.pickUpItem targetID EntityID.player
                    yield! Model.moveToward selectedTile EntityID.player
                    yield! environmentTurn
                } |> Ok

            | Some _ ->
                msgCE {
                    yield! Model.moveToward selectedTile EntityID.player
                    yield! environmentTurn
                } |> Ok

            | _ ->
                Model.PutLog "You cannot do that" |> Error

    } |> Result.merge id
}
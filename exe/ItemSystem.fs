module ItemSystem

open Rogue.Lib
open ProjectUtils
open Accessor
open RayPlatform
open Model

type IItem =
    inherit IBehaviour
    abstract member Value: Accessor<IItem, int>

type IContainer =
    inherit IBehaviour
    inherit seq<IItem>
    abstract member Contents: Accessor<IContainer, List<IItem>>

type Item =
    { _name: string; _position: IntVec; _token: char; _value: int }
    static member Make (name, token, value, ?position) =
        { _name = name; _token = token; _value = value; _position = defaultArg position IntVec.Zero }

    interface IItem with
        member this.Name =
            { Get = this._name; Change = fun f -> { this with _name = f this._name} }

        member this.Position =
            { Get = this._position; Change = fun f -> { this with _position = f this._position} }

        member this.Token =
            { Get = this._token; Change = fun f -> { this with _token = f this._token } }

        member this.Value =
            { Get = this._value; Change = fun f -> { this with _value = f this._value } }

type Chest =
    { _name: string
    ; _position: IntVec
    ; _token: char
    ; _contents: List<IItem>
    }
    interface seq<IItem> with
        member this.GetEnumerator (): System.Collections.Generic.IEnumerator<IItem> =
            (this._contents :> seq<IItem>).GetEnumerator()
        member this.GetEnumerator (): System.Collections.IEnumerator = 
            (this :> seq<IItem>).GetEnumerator()

    interface IContainer with
        member this.Name =
            { Get = this._name; Change = fun f -> { this with _name = f this._name } }

        member this.Position =
            { Get = this._position; Change = fun f -> { this with _position = f this._position } }

        member this.Token =
            { Get = this._token; Change = fun f -> { this with _token = f this._token } }

        member this.Contents =
            { Get = this._contents; Change = fun f -> { this with _contents = f this._contents } }

let addItem (item: IItem) (entityID: EntityID) = msgCE {
    let! model: Model = Msgs.identity
    let entity = model[entityID]

    match entity.Get with
    | Some (:? IContainer as container) ->
        yield entity <-- Some (container.Contents <-- item :: container.Contents.Get :> IBehaviour)
        yield! Model.PutLog $"{item.Name.Get} has been added to {container.Name.Get}'s inventory"
        
    | _ ->
        yield! Model.PutLog $"ERROR: there is no container with the ID {entityID}"
}

let pickUpItem (itemID: EntityID) (containerID: EntityID) = msgCE {
    let! model: Model = Msgs.identity

    match model[itemID].Get, model[containerID].Get with
    | Some (:? IItem as item), Some (:? IContainer) ->
        yield! Model.DestroyEntity itemID
        yield! addItem item containerID

    | _ -> yield! Model.PutLog $"ERROR: there is no item with the ID {itemID} and/or there is no container with the ID {containerID}"
}
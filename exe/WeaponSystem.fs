module WeaponSystem

open Rogue.Lib
open Accessor

type IWeapon =
    abstract member Damage: int

let fists =
    { new IWeapon with member _.Damage = 3 }

type Sword =
    { _name: string
    ; _position: IntVec
    ; _token: char
    ; _value: int
    ; _damage: int
    }
    interface ItemSystem.IItem with
        member this.Name =
            { Get = this._name; Change = fun f -> { this with _name = f this._name } }
        member this.Position =
            { Get = this._position; Change = fun f -> { this with _position = f this._position } }
        member this.Token =
            { Get = this._token; Change = fun f -> { this with _token = f this._token } }
        member this.Value =
            { Get = this._value; Change = fun f -> { this with _value = f this._value } }

    interface IWeapon with
        member this.Damage =
            this._damage
open Creature

open Model
open View
open Subscription

open Rogue.Lib
open RayPlatform

type Creature =
    { _position: IntVec
    ; _token: char
    ; _strength: int
    ; _health: int
    }
    interface Update.ICharacterSheet with
        member this.Position =
            { Accessor.Get = this._position; Accessor.Change = fun f -> { this with _position = f this._position }}
        member this.Token =
            { Accessor.Get = this._token; Accessor.Change = fun f -> { this with _token = f this._token }}
        member this.Strength =
            { Accessor.Get = this._strength; Accessor.Change = fun f -> { this with _strength = f this._strength }}
        member this.Health =
            { Accessor.Get = this._health; Accessor.Change = fun f -> { this with _health = f this._health }}


let init =
    { _map = Set.empty
    ; _entities = Map.empty
    ; _seed = RandomPure.Seed 69
    }
    |> Model.genNewMap
    |> fun m ->
        { m with
            _entities =
                Map
                    [ EntityID.player, { _token = '@'; _position = Seq.randomChoice m.Map.Get; _strength = 11; _health = 100 }
                    ; (enum<EntityID> 10), { _token = 'g'; _position = Seq.randomChoice m.Map.Get; _strength = 10; _health = 100 }
                    ]
        }

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    subscriptions
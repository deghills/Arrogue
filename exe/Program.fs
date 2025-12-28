open Model

open ItemSystem
open CharacterSystem

open View
open Subscription

open Rogue.Lib
open ProjectUtils
open RayPlatform

let init =
    Writer.writer {
        do! Model.GenNewMap |> Writer.write
        do! Model.SpawnEntityOnRandomTile (Creature.Make ("arrow", '@', 12, 100), EntityID.player) |> Writer.write
        do! Model.SpawnEntityOnRandomTile (Creature.Make ("goblin", 'g', 5, 100)) |> Writer.write
        do! Model.SpawnEntityOnRandomTile (Creature.Make ("goblin", 'g', 5, 100)) |> Writer.write
        do! Model.SpawnEntityOnRandomTile (Item.Make ("a coin", '$', 1)) |> Writer.write
        return Model.Make (RandomPure.Seed (System.Random().Next()))
    } |> Writer.unwrap

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    subscriptions
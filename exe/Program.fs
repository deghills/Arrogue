open Creature

open Model
open View
open Subscription

open Rogue.Lib
open RayPlatform

let init =
    Model.empty
    |> Model.genNewMap
    |> fun m ->
        spawnCreature EntityID.player { Token = '@'; Pos = Seq.randomChoice m.Map } Creature.dummy m

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    subscriptions
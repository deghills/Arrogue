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
        m
        |> spawnCreature EntityID.player { Token = '@'; Pos = Seq.randomChoice m.Map } Creature.dummy
        |> spawnCreature (enum<EntityID> 10) { Token = 'g'; Pos = Seq.randomChoice m.Map } Creature.dummy

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    subscriptions
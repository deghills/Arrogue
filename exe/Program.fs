open Creature

open Model
open View
open Subscription

open Rogue.Lib
open ProjectUtils
open RayPlatform
open BSP

let init =
    Model.empty
    |> Model.genNewMap
    |> fun m ->
        Model.creaturesLens.update (Map.add CreatureID.player (Creature.spawnDummy (Seq.randomChoice m.Map))) m

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    subscriptions
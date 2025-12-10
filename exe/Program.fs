open Creature

open Model
open Update
open View
open Subscription

open Rogue.Lib
open RayPlatform
open BSP

let theMap = BSP.genRandomMap (Bounds.t (0, 64, 0, 32)) 4 4 |> Seq.collect Bounds.containedPoints |> Set
let randomSpawnLocation() = Seq.randomChoice theMap



let init =
    { Creatures =
        Map
            [ CreatureID.player, { Pos = randomSpawnLocation(); Token = '@'; Stats = { Health = 100; Strength = 10 } }
            ; enum<CreatureID> 0, { Pos = randomSpawnLocation(); Token = 'g'; Stats = { Health = 100; Strength = 11 } } ]
    ; Map =
        theMap
    }

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    subscriptions
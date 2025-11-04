open Rogue.Lib
open RayPlatform
open Creature
open Model
open Update
open View
open Subscription

let init =
    { Creatures =
        Map
            [ CreatureID.player, { Pos = IntVec.Vec (20, 20); Token = '@'; Stats = { Health = 100; Strength = 10 } }
            ; enum<CreatureID> 0, { Pos = IntVec.Vec (15, 15); Token = 'g'; Stats = { Health = 100; Strength = 11 } } ]
    ; Walls =
        Set
            [ for j in 0..20 do if j <> 13 && j <> 0 then yield Vec (17, j) ]
    }

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    update
    subscriptions
open Model
open View
open Subscription

open Rogue.Lib
open ProjectUtils
open RayPlatform

let init =
    Writer.writer {
        do! Model.GenNewMap |> Writer.write
        do! Model.SpawnEntityOnRandomTile (Update.Creature (11, 100, '@', IntVec.Zero), EntityID.player) |> Writer.write
        do! Model.SpawnEntityOnRandomTile (Update.Creature (10, 100, 'g', IntVec.Zero)) |> Writer.write
        do! Model.PutLog "hey guys" |> Writer.write
        do! Model.PutLog "can you hear me?" |> Writer.write
        return Model.Empty
    } |> Writer.unwrap

RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    subscriptions
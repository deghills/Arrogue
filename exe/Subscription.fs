module Subscription

open Rogue.Lib
open ProjectUtils
open RayPlatform
open Accessor

open Model

let subscriptions (frame: FrameContext) (model: Model) =
    [ match model .> (_.Entities $ Map.itemLens EntityID.player) with
        | Some player ->
            if frame[KeyboardKey.KEY_W].IsPressed then
                yield CharacterSystem.playerGenericAction (player.Position.Get + IntVec.Vec (0, -1))

            if frame[KeyboardKey.KEY_A].IsPressed then
                yield CharacterSystem.playerGenericAction (player.Position.Get + IntVec.Vec (-1, 0))

            if frame[KeyboardKey.KEY_S].IsPressed then
                yield CharacterSystem.playerGenericAction (player.Position.Get + IntVec.Vec (0, 1))

            if frame[KeyboardKey.KEY_D].IsPressed then
                yield CharacterSystem.playerGenericAction (player.Position.Get + IntVec.Vec (1, 0))
        | None -> ()

    ; if frame[KeyboardKey.KEY_EQUAL].IsPressed then
        yield Msg (fun m -> m.Zoom <-- min (m.Zoom.Get + 2) 32)

    ; if frame[KeyboardKey.KEY_MINUS].IsPressed then
        yield Msg (fun m -> m.Zoom <-- max (m.Zoom.Get - 2) 16)

    ; let fps = (1f / frame.Frametime) in
        if fps < 60f then yield Model.PutLog $"Low FPS warning: {fps}FPS"
    
    ]
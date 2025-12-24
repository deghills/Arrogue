module Subscription

open Rogue.Lib
open RayPlatform

open Model

let subscriptions (frame: FrameContext) (state: Model) =
    match state.FindEntity EntityID.player with
    | Some player ->
        [ if frame[KeyboardKey.KEY_W].IsPressed then
            yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (0, -1))

        ; if frame[KeyboardKey.KEY_A].IsPressed then
            yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (-1, 0))

        ; if frame[KeyboardKey.KEY_S].IsPressed then
            yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (0, 1))

        ; if frame[KeyboardKey.KEY_D].IsPressed then
            yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (1, 0))
        ]
    | None -> []
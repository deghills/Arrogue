module Subscription

open Rogue.Lib
open RayPlatform

open Model

let subscriptions (state: Model) =
    fun (tick: TickInfo) ->
        match state.FindEntity EntityID.player with
        | Some player ->
            [ if tick[KeyboardKey.KEY_W].IsPressed then
                yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (0, -1))

            ; if tick[KeyboardKey.KEY_A].IsPressed then
                yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (-1, 0))

            ; if tick[KeyboardKey.KEY_S].IsPressed then
                yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (0, 1))

            ; if tick[KeyboardKey.KEY_D].IsPressed then
                yield Update.playerGenericAction (player.Position.Get + IntVec.Vec (1, 0))
            ]
        | None -> []//[ PlatformMsgs.quit ]
    |> OnTick
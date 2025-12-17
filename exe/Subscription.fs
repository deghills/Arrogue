module Subscription

open Rogue.Lib
open RayPlatform
open Update
open Creature

open Model


let subscriptions (state: Model) =
    { new RayPlatform.ISubscription<Msg, Model> with
        member _.OnTick tick =
            match state.Tiles.TryFind EntityID.player with
            | Some { Pos = playerPos } ->
                [ if tick[KeyboardKey.KEY_W].IsPressed then
                    yield GenericAction (EntityID.player, playerPos + IntVec.Vec (0, -1))
                ; if tick[KeyboardKey.KEY_A].IsPressed then
                    yield GenericAction (EntityID.player, playerPos + IntVec.Vec (-1, 0))
                ; if tick[KeyboardKey.KEY_S].IsPressed then
                    yield GenericAction (EntityID.player, playerPos + IntVec.Vec (0, 1))
                ; if tick[KeyboardKey.KEY_D].IsPressed then
                    yield GenericAction (EntityID.player, playerPos + IntVec.Vec (1, 0))

                ; if tick[KeyboardKey.KEY_SPACE].IsPressed then
                    yield EnvironmentTurn
                ]
            | None ->
                [ PlatformMsgs.quit ]
    }
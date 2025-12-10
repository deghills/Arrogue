module Subscription

open Rogue.Lib
open RayPlatform
open Msg
open Update
open Creature
open Model

let subscriptions state =
    { new RayPlatform.Subscription.ISubscription<Msg, Model> with
        member _.OnTick tick =
            match state.Creatures.TryFind CreatureID.player with
            | Some player ->
                [ if tick[KeyboardKey.KEY_W].IsPressed then
                    yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (0, -1))
                ; if tick[KeyboardKey.KEY_A].IsPressed then
                    yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (-1, 0))
                ; if tick[KeyboardKey.KEY_S].IsPressed then
                    yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (0, 1))
                ; if tick[KeyboardKey.KEY_D].IsPressed then
                    yield GenericAction (CreatureID.player, player.Pos + IntVec.Vec (1, 0))

                ; if tick[KeyboardKey.KEY_SPACE].IsPressed then
                    yield EnvironmentTurn
                ]
            | None ->
                [ Quit ]
    }
module Creature

open Rogue.Lib

type Creature =
    { Health    : int
    ; Strength  : int
    }

let attack (c1: Creature) (c2: Creature) : option<Creature> =
    match { c2 with Health = c2.Health - c1.Strength } with
    | dead when dead.Health < 1 -> None
    | alive -> Some alive

let dummy =
    { Health = 100
    ; Strength = 10
    }
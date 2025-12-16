module Creature

open Rogue.Lib

type CreatureID =
    | player = -69

type CreatureSheet =
    { Health: int
    ; Strength: int
    }

type Creature =
    { Pos: IntVec
    ; Token: char
    ; Stats: CreatureSheet
    }
    static member View { Pos = p; Token = token } = p, token
    static member Attack (c1: Creature) (c2: Creature) : option<Creature> =
        let (|Dead|_|) c = c.Stats.Health < 1
        match { c2 with Stats.Health = c2.Stats.Health - c1.Stats.Strength } with
        | Dead -> None
        | alive -> Some alive

let spawnDummy pos =
    { Pos = pos
    ; Token = '@'
    ; Stats = { Health = 100; Strength = 10 }
    }
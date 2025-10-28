namespace Rogue.Lib.Game

open Rogue.Lib

type CreatureID =
    | player = -69

type CreatureSheet =
    { Health: int
    ; Strength: int
    }

type Creature =
    { Token: char
    ; Stats: CreatureSheet
    }
    static member Attack (c1: Creature) (c2: Creature) : Creature option =
        { c2 with Stats.Health = c2.Stats.Health - c1.Stats.Strength }
        |> function dead when dead.Stats.Health < 1 -> None | alive -> Some alive

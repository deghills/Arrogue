namespace Rogue.Lib

open ProjectUtils

type Snapshot<'s, 'a> =
    { View: 'a
    ; Update: ('a -> 'a) -> 's
    }

module Lens =
    let compose (outer: 'a -> Snapshot<'a, 'b>, inner: 'b -> Snapshot<'b, 'c>) a =
        let outerView = outer >> _.View
        let innerView = inner >> _.View
        let outerUpdate = outer >> _.Update
        let innerUpdate = inner >> _.Update

        let finalView = outerView >> innerView
        let finalUpdate = flip innerUpdate >> flip outerUpdate
        { View = finalView a; Update = finalUpdate}

(*
[<Struct>]
type Lens<'s, 'a> =
    { Look: 's -> 'a
    ; Change: ('a -> 'a) -> 's -> 's
    }
    member this.Set a = this.Change (konst a)
    
    static member Compose (outer: Lens<'a, 'b>) (inner: Lens<'b, 'c>) =
        { Look = outer.Look >> inner.Look
        ; Change = inner.Change >> outer.Change
        }

        *)
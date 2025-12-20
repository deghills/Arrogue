namespace Rogue.Lib

module Accessor =
    type Accessor<'structure, 'focus> =
        { Get: 'focus
        ; Change: ('focus -> 'focus) -> 'structure
        }
        member this.Set x = this.Change (fun _ -> x)
        //static member ( !! ) { Get = get } = get

    let ( $ ) (leftAccess: 'a -> Accessor<'a, 'b>) (rightAccess: 'b -> Accessor<'b, 'c>) (a: 'a) : Accessor<'a, 'c> =
        let l1 = leftAccess a
        let l2 = rightAccess (l1.Get)

        let change f =
            let set1 = l2.Change f
            l1.Set set1

        { Get = l2.Get
        ; Change = change
        }

    let ( <-- ) (access: Accessor<'a, 'b>) (value: 'b) = access.Set value
    let ( <-* ) (access: Accessor<'a, 'b>) (f: 'b -> 'b) = access.Change f
    let ( .> ) (a: 'a) (access: 'a -> Accessor<'a, 'b>) = (access a).Get
namespace Rogue.Lib

type Lens<'structure, 'focus> =
    { get: 'structure -> 'focus
    ; change: ('focus -> 'focus) -> 'structure -> 'structure
    }
    member this.set x = this.change (fun _ -> x)

    static member ( $ ) ({ get = leftGet; change = leftChange }, { get = rightGet; change = rightChange }) =
        { get = leftGet >> rightGet; change = rightChange >> leftChange}

    static member ( .> ) (a, b) =
        b.get a

    static member ( <-- ) (l: Lens<'a, 'b>, v) = l.set v

    static member ( <-* ) (l, f) = l.change f

    (*static member ( <?* ) (l: Lens<'structure, option<'t>>, f: 't -> 't) =
        l <-* (Option.map f)*)

    static member ( <?- ) (l: Lens<'structure, option<'t>>, t: 't) =
        l.set (Some t)
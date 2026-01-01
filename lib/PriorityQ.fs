namespace Rogue.Lib.Data

open System

type PriorityQ<'t when 't : comparison> =
    val private tree: BinaryTree<'t>
    val private n: uint
    private new (tree, n) = { tree = tree; n = n }
    new() = { tree = Nil; n = 0u }

    override this.ToString() = 
        $"tree size: {this.n}\n{this.tree.ToString()}"

    static member Insert (insertion: 't) (parent: PriorityQ<'t>) =
        let rec aux insertion n =
            let treeIsBalanced = not (System.UInt32.IsEvenInteger n) in

            function
            | Branch (l, payload, r) when treeIsBalanced ->
                let index_child = n >>> 1

                if insertion < payload then
                    ( aux payload index_child l
                    , insertion
                    , r
                    ) |> Branch
                else
                    ( aux insertion index_child l
                    , payload
                    , r
                    ) |> Branch

            | Branch (l, payload, r) ->
                let index_child = (n >>> 1) - 1u

                if insertion < payload then
                    ( l
                    , insertion
                    , aux payload index_child r
                    ) |> Branch
                else
                    ( l
                    , payload
                    , aux insertion index_child r
                    ) |> Branch

            | Nil -> BinaryTree<'t>.Return insertion
    
        in PriorityQ (aux insertion parent.n parent.tree, parent.n + 1u)

    static member InsertMany (insertions: seq<'t>) parent =
        Seq.fold
            (fun acc insertion -> PriorityQ.Insert (insertion: 't) acc)
            parent
            insertions

    static member Pop (heap: PriorityQ<'t>) =

        let rec extractPreviousInsertion (n: uint) (tree: BinaryTree<'t>) =
            let treeIsBalanced = not (UInt32.IsEvenInteger n)
            match tree with
            | Nil -> None

            | Branch (Nil, valueToExtract, Nil) ->
                Some (Nil, valueToExtract)

            | Branch (l, v, r) when treeIsBalanced ->
                extractPreviousInsertion (n >>> 1) r
                |> Option.map
                    (function r', valueToExtract -> Branch (l, v, r'), valueToExtract)

            | Branch (l, v, r) ->
                extractPreviousInsertion (n >>> 1) l
                |> Option.map
                    (function l', valueToExtract -> Branch (l', v, r), valueToExtract)

        let rec reconcile tree =
            match tree with
            | Nil -> Nil

            | Branch (Branch (lA, vA, rA), v, Branch(lB, vB, rB)) when v > vA && v > vB ->
                if vA < vB then
                    ( Branch (lA, v, rA) |> reconcile
                    , vA
                    , Branch (lB, vB, rB)
                    ) |> Branch
                else
                    ( Branch (lA, vA, rA)
                    , vB
                    , Branch (lB, v, rB) |> reconcile
                    ) |> Branch

            | Branch (Branch (lA, vA, rA), v, b) when v > vA ->
                ( Branch (lA, v, rA) |> reconcile
                , vA
                , b
                ) |> Branch

            | Branch (a, v, Branch (lB, vB, rB)) when v > vB ->
                ( a
                , vB
                , Branch (lB, v, rB) |> reconcile
                ) |> Branch

            | _ -> tree

        match extractPreviousInsertion heap.n heap.tree with
        | Some (Branch (l,popped, r), previousInsertion) ->
            let withInversion = Branch (l, previousInsertion, r)

            ( PriorityQ (reconcile withInversion, heap.n - 1u)
            , Some popped
            )

        | Some (Nil, previousInsertion) ->
            PriorityQ (Nil, 0u), Some previousInsertion

        | None -> heap, None

    static member PopAll (heap: PriorityQ<'t>) =
        let rec aux acc heap =
            match PriorityQ.Pop heap with
            | heap', Some popped ->
                aux (popped :: acc) heap'

            | _, None -> heap, List.rev acc
        in aux [] heap |> snd

    new(insertions: seq<'t>) =
        let result =
            PriorityQ()
            |> PriorityQ.InsertMany insertions
        { tree = result.tree; n = result.n }

(*member this.Verify() =
    let rec aux (counter: int) (heaps: List<BinaryTree<'t>>) =
        match heaps with
        | [] when counter = 0 -> let () = printfn "this was a good heap" in this

        | [] ->
            printfn "%O" this
            failwith $"_.n was off by {counter}"

        | Nil :: remainingHeaps -> aux counter remainingHeaps

        | Branch (l, v, r) :: remainingHeaps ->
            match l, r with
            | Branch (_, smallerChild, _), _ | _, Branch (_, smallerChild, _) when smallerChild < v ->
                printfn "%O" this
                failwith $"found inversion: parent {v} was greater than child {smallerChild}"

            | Nil, Nil -> aux (counter - 1) remainingHeaps

            | Nil, Branch (_, _, _) ->
                aux (counter - 1) (r :: remainingHeaps)

            | Branch (_, _, _), Nil ->
                aux (counter - 1) (l :: remainingHeaps)

            | Branch (_, _, _), Branch (_, _, _) ->
                aux (counter - 1) (l :: r :: remainingHeaps)

    in aux (int this.n) [this.tree]*)
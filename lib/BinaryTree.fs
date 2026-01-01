namespace Rogue.Lib.Data

type BinaryTree<'t> =
    | Nil
    | Branch of BinaryTree<'t> * 't * BinaryTree<'t>

    static member Return payload =
        Branch (Nil, payload, Nil)

    override this.ToString() =
        let rec aux (stringAcc: string) = function
            | [||] -> stringAcc

            | currentRow ->
                let thisRowAsString =
                    currentRow
                    |> Array.map
                        (function
                        | Nil -> "Nil"
                        | Branch (_, v, _) -> string v)
                    |> String.concat " | "

                let nextRow =
                    currentRow
                    |> Array.collect
                        (function
                        | Nil -> [||]
                        | Branch (l, _, r) -> [| l; r |]
                        )

                aux
                    (stringAcc + thisRowAsString + "\n")
                    nextRow

        aux "" [|this|]
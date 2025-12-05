module View

open Rogue.Lib
open Model
open Creature

open ZeroElectric.Vinculum

let cellSize = 30

let drawChar (Vec (x, y)) (chr: char) =
    let fontSize = (cellSize * 13) / 10
    Raylib.DrawText (string chr, x * cellSize, y * cellSize, fontSize, Raylib.RAYWHITE)

let view state =
    if state.Creatures.ContainsKey CreatureID.player |> not then
        Raylib.DrawText
            ( "the player has died!\npress any key to close window"
            , 0
            , 0
            , 20
            , Raylib.RAYWHITE
            )
    else
        state.Creatures
        |> Map.toSeq
        |> Seq.fold
            (fun acc (_, c) -> Map.add c.Pos c.Token acc)
            Map.empty
        |> fun charmap ->
            Seq.fold
                (fun acc wallPos -> Map.add wallPos '=' acc)
                charmap
                state.Walls
        |> Map.iter drawChar
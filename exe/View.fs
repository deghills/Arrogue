module View

open ZeroElectric.Vinculum
open Rogue.Lib
open ProjectUtils
open Model
open Creature


let cellSize = 30

let drawChar (Vec (x, y)) (chr: char) =
    let fontSize = (cellSize * 13) / 10
    Raylib.DrawText (string chr, x * cellSize, y * cellSize, fontSize, Raylib.RAYWHITE)

let view state =
    if state.Creatures.ContainsKey CreatureID.player |> not
    
    then
        Raylib.DrawText
            ( "the player has died!\npress any key to close window"
            , 0
            , 0
            , 20
            , Raylib.RAYWHITE
            )

    else
        Seq.fold (fun acc mapTilePos -> Map.add mapTilePos '-' acc) Map.empty state.Map
        |> fun mapTiles ->
            Seq.fold (fun acc (_, creature) -> Map.add creature.Pos creature.Token acc) mapTiles (Map.toSeq state.Creatures)
        |> Map.iter drawChar
        
        
        (*|> Seq.fold
            (fun acc (_, creature) -> Map.add creature.Pos creature.Token acc)
            Map.empty
        |> fun charmap ->
            Seq.fold
                (fun acc wallPos -> Map.add wallPos '-' acc)
                charmap
                state.Walls*)
        
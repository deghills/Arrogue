module View

open Rogue.Lib
open RayPlatform
open ProjectUtils
open Model
open Creature


let cellSize = 30

let drawChar pos (chr: char) =
    let fontSize = (cellSize * 13) / 10
    RayPlatform.View.text (string chr) pos fontSize Colours.rayWhite

let view state =
    if state.Creatures.ContainsKey CreatureID.player |> not
    
    then
        View.text
            "the player has died!\npress any key to close window"
            (Vec (0, 0))
            20
            Colours.rayWhite

    else
        Seq.fold
            (fun acc mapTilePos -> Map.add mapTilePos '-' acc)
            Map.empty
            state.Map
        |> fun mapTiles ->
            Seq.fold
                (fun acc (_, creature) -> Map.add creature.Pos creature.Token acc)
                mapTiles
                (Map.toSeq state.Creatures)
        |> Seq.map (function KeyValue (pos, chr) -> drawChar (pos * cellSize) chr)
        |> Seq.fold (RayPlatform.View.compose) RayPlatform.View.zero
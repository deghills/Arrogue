module View

open Rogue.Lib
open RayPlatform
open Model

let cellSize = 30

let drawChar pos (chr: char) =
    let fontSize = (cellSize * 13) / 10
    RayPlatform.Viewables.text (string chr) pos fontSize Colours.rayWhite

let view (state: Model) =
    if not (state.Creatures.ContainsKey EntityID.player)
    
    then
        Viewables.text
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
                (fun acc (_, tile) -> Map.add tile.Pos tile.Token acc)
                mapTiles
                (Map.toSeq state.Tiles)
        |> Seq.map (function KeyValue (pos, chr) -> drawChar (pos * cellSize) chr)
        |> Seq.fold (Viewables.compose) RayPlatform.Viewables.zero
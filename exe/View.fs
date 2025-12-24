module View

open Rogue.Lib
open RayPlatform
open Model
open Viewables

let cellSize = 30

let view (frame: FrameContext) (state: Model) =
    let fontSize = cellSize * 13 / 10

    seq {
        let allTokens =
            Seq.fold
                (fun acc mapTilePos -> Map.add mapTilePos '-' acc)
                Map.empty
                state.Map.Get
            |> fun mapTiles ->
                Seq.fold
                    (fun acc (tile: IBehaviour) -> Map.add tile.Position.Get tile.Token.Get acc)
                    mapTiles
                    (state.Entities.Get.Values)

        for KeyValue (pos, token) in allTokens do
            yield Text (string token, pos * cellSize, fontSize, Colours.green) :> IViewable<Model>

        let twoThirdsIn = frame.RenderWidth * 2 / 3
        let logWindow = { x = twoThirdsIn; y = 0; width = frame.RenderWidth / 3; height = frame.RenderHeight; colour = Colours.black; isSolid = true }
        yield logWindow
        yield { logWindow with isSolid = false; colour = Colours.green }

        yield!
            state.Logs
            |> Seq.mapi
                (fun i log ->
                    Text
                        ( log
                        , twoThirdsIn
                        , frame.RenderHeight - (i + 1) * fontSize
                        , fontSize
                        , Colours.green
                        ) :> IViewable<Model>
                )
    }
    |> Seq.fold IViewable.Compose Viewables.empty<Model>
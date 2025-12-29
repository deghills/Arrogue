module View

open Rogue.Lib
open RayPlatform
open Model
open Viewables

let view (frame: FrameContext) (model: Model) =
    let tileSize = model.Zoom.Get * 13 / 10
    let fontSize = frame.RenderWidth / 70
    let twoThirdsIn = frame.RenderWidth * 2 / 3
    let tilesPerHorz = twoThirdsIn / tileSize + 1
    let tilesPerVert = frame.RenderHeight / tileSize + 1
    let middle = Vec (tilesPerHorz, tilesPerVert) / 2

    seq {
        match model[EntityID.player].Get with
        | None -> ()
        | Some player ->
            let toPlayerLocalCoordinates =
                middle - player.Position.Get

            for mapTile in model.Map.Get do
                yield
                    { Body = "-"
                    ; Pos = (mapTile + toPlayerLocalCoordinates) * tileSize
                    ; FontSize = tileSize
                    ; Colour = Colours.green
                    } :> IViewable<Model>

            for KeyValue (_, entity) in model.Entities.Get do
                yield
                    { Body = string entity.Token.Get
                    ; Pos = (entity.Position.Get + toPlayerLocalCoordinates) * tileSize
                    ; FontSize = tileSize
                    ; Colour = Colours.green
                    }

        let logWindow = { x = twoThirdsIn; y = 0; width = frame.RenderWidth / 3; height = frame.RenderHeight; colour = Colours.black; isSolid = true }
        
        yield logWindow
        yield { logWindow with isSolid = false; colour = Colours.green }
        yield!
            model.Logs.Get
            |> Seq.mapi
                (fun i log ->
                    { Body = log
                    ; Pos = Vec ( twoThirdsIn, frame.RenderHeight - (i + 1) * fontSize )
                    ; FontSize = fontSize
                    ; Colour = Colours.green
                    } :> IViewable<Model>
                )
    } |> Seq.fold IViewable.Compose Viewables.empty<Model>
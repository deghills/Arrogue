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
    let playerPos =
        model[EntityID.player].Get
        |> Option.map _.Position.Get
        |> Option.defaultValue IntVec.Zero

    let worldToScreen (point: IntVec) =
        (point + (middle - playerPos)) * tileSize

    let screenToWorld (point: IntVec) =
        point / tileSize - (middle - playerPos)

    seq {
        match model[EntityID.player].Get with
        | None -> ()
        | Some player ->
            for KeyValue (pos, token)
                in  [ for mapTile in model.Map.Get do yield (mapTile, '-')
                    ; for KeyValue (_, entity) in model.Entities.Get do yield (entity.Position.Get, entity.Token.Get)
                    ]
                    |> Map
                
                do yield
                    { Body = string token
                    ; Pos = worldToScreen pos
                    ; FontSize = tileSize
                    ; Colour = Colours.green
                    ; OnLeftClick = CharacterSystem.playerGenericAction pos
                    ; OnRightClick = Msgs.identity
                    } :> IViewable<Model>

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
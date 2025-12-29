namespace Rogue.Lib
open ProjectUtils

module RayPlatform =
    open ZeroElectric.Vinculum

    type KeyboardKey = ZeroElectric.Vinculum.KeyboardKey

    [<Struct>]
    type ButtonInfo =
        val private getPressed: unit -> bool
        val private getReleased: unit -> bool
        val private getDown: unit -> bool
        internal new (k: KeyboardKey) =
            { getPressed = fun () -> Raylib.IsKeyPressed k
            ; getReleased = fun () -> Raylib.IsKeyReleased k
            ; getDown = fun () -> Raylib.IsKeyDown k
            }
        internal new (m: MouseButton) =
            { getPressed = fun () -> Raylib.IsMouseButtonPressed m
            ; getReleased = fun () -> Raylib.IsMouseButtonReleased m
            ; getDown = fun () -> Raylib.IsMouseButtonDown m
            }
        member this.IsPressed = this.getPressed()
        member this.IsReleased = this.getReleased()
        member this.IsDown = this.getDown()

    type Config =
        { Resolution: int*int
        ; FPS: int
        ; ShowFPS: bool
        ; TextSize: int
        ; BackgroundColour: Color
        ; HideCursor: bool
        ; Fullscreen: bool
        } with
        static member Default =
            { Resolution = 1920, 1080
            ; FPS = -1
            ; ShowFPS = true
            ; TextSize = 16
            ; BackgroundColour = Raylib.BLACK
            ; HideCursor = false
            ; Fullscreen = true
            }

    type FrameContext internal () =
        let pressedKeys =
            let rec aux acc =
                match Raylib.GetKeyPressed() with
                0 -> acc | i -> aux (enum<KeyboardKey> i :: acc)
            in aux []

        member _.Frametime = Raylib.GetFrameTime()
        member _.MouseDelta = Raylib.GetMouseDelta()
        member _.MousePosition = Raylib.GetMousePosition()
        member _.PressedKeys = pressedKeys
        member _.RenderHeight = Raylib.GetRenderHeight()
        member _.RenderWidth = Raylib.GetRenderWidth()

        member _.Item(k: KeyboardKey) = ButtonInfo k
        member _.Item(m: MouseButton) = ButtonInfo m

    type Colour = ZeroElectric.Vinculum.Color
    module Colours =
        let rayWhite = Raylib.RAYWHITE
        let green = Raylib.GREEN
        let beige = Raylib.BEIGE
        let darkGrey = Raylib.DARKGRAY
        let black = Raylib.BLACK

    type Msg<'model> = Msg of ('model -> 'model)

    type MsgBuilder() =
        member _.Bind(Msg (f: 'model -> 'model), k: 'model -> Msg<'model>) =
            Msg
                (fun m ->
                    let m' = f m
                    let (Msg f') = k m'
                    f' m'
                )

        member _.Yield model =
            Msg (fun _ -> model)

        member this.YieldFrom msg =
            this.Bind (msg, this.Yield)

        member this.Combine(a: Msg<'model>, b: Msg<'model>) =
            this.Bind (a, fun _ -> b)

        member _.Delay(f: unit -> Msg<'model>) = f()

        member _.Zero() = Msg id

        member this.For(xs: seq<'x>, k: 'x -> Msg<'model>) =
            xs
            |> Seq.map k
            |> Seq.fold (fun a b -> this.Combine(a, b)) (this.Zero())

    let msgCE = MsgBuilder()

    type IViewable<'model> = interface
        abstract member View : unit -> List<Msg<'model>>
        static member Compose (leftView: #IViewable<'model>) (rightView: #IViewable<'model>) =
            { new IViewable<'model> with member _.View() = leftView.View() @ rightView.View() }
    end
    
    module Msgs =
        let identity = Msg id

        let quit<'model>: Msg<'model> =
            let () =
                Raylib.CloseWindow()
                printfn "GAME OVER"
            exit 0

        let changeWindowSize horz vert =
            let () = Raylib.SetWindowSize(horz, vert)
            identity

    module Viewables =
        let empty<'model> = { new IViewable<'model> with member _.View() = [] }

        [<Struct>]
        type Rect<'model> =
            { x: int
            ; y: int
            ; width: int
            ; height: int
            ; colour: Colour
            ; isSolid: bool
            }
            interface IViewable<'model> with
                member this.View() =
                    let () =
                        (this.x, this.y, this.width, this.height, this.colour)
                        |> if this.isSolid then Raylib.DrawRectangle else Raylib.DrawRectangleLines
                    []

            static member OfBounds (bounds: Bounds, colour: Colour, ?isSolid) =
                { x = bounds.MinX
                ; y = bounds.MinY
                ; width = bounds.MaxX - bounds.MinX
                ; height = bounds.MaxY - bounds.MinY
                ; colour = colour
                ; isSolid = defaultArg isSolid false
                }

        type Text<'model> =
            { Body: string
            ; Pos: IntVec
            ; FontSize: int
            ; Colour: Colour
            } interface IViewable<'model> with
                member this.View() = let () = Raylib.DrawText(this.Body, this.Pos.X, this.Pos.Y, this.FontSize, this.Colour) in []

        type TextButton<'model> =
            { Body: string
            ; Pos: IntVec
            ; FontSize: int
            ; Colour: Colour
            ; OnLeftClick: Msg<'model>
            ; OnRightClick: Msg<'model>
            } interface IViewable<'model> with
                member this.View() =
                    let boundingRect =
                        { x = this.Pos.X
                        ; y = this.Pos.Y
                        ; width = Raylib.MeasureText(this.Body, this.FontSize) |> max this.FontSize
                        ; height = this.FontSize
                        ; colour = Colours.green
                        ; isSolid = false
                        }
                    
                    let bounds =
                        ( boundingRect.x
                        , boundingRect.x + boundingRect.width
                        , boundingRect.y
                        , boundingRect.y + boundingRect.height
                        ) |> Bounds
                    
                    let isHovered = Bounds.ContainsPoint (Raylib.GetMouseX(), Raylib.GetMouseY()) bounds

                    Raylib.DrawText (this.Body, this.Pos.X, this.Pos.Y, this.FontSize, if isHovered then Raylib.PINK else this.Colour)
                    if isHovered then Raylib.DrawRectangleLines(boundingRect.x, boundingRect.y, boundingRect.width, boundingRect.height, Colours.green)

                    [ if isHovered && Raylib.IsMouseButtonPressed(MouseButton.MOUSE_BUTTON_LEFT) then
                        yield this.OnLeftClick

                    ; if isHovered && Raylib.IsMouseButtonPressed(MouseButton.MOUSE_BUTTON_RIGHT) then
                        yield this.OnRightClick
                    ]

    let run
        (cfg: Config)
        (init: 'model * List<Msg<'model>>)
        (view: FrameContext -> 'model -> IViewable<'model>)
        (subscription: FrameContext -> 'model -> List<Msg<'model>>)
    
        = do
        Raylib.InitWindow (fst cfg.Resolution, snd cfg.Resolution, "a game to be played")
        Raylib.SetTargetFPS cfg.FPS
        RayGui.GuiSetStyle (int GuiControl.DEFAULT, int GuiDefaultProperty.TEXT_SIZE, cfg.TextSize)
        if cfg.HideCursor then Raylib.HideCursor()
        if cfg.Fullscreen then Raylib.ToggleFullscreen()
    
        let rayTick model =
            [ Raylib.BeginDrawing()
            ; Raylib.ClearBackground cfg.BackgroundColour
            ; if cfg.ShowFPS then Raylib.DrawFPS(0, 0)

            ; let frame = FrameContext() in ()

            ; yield!
                (view frame model).View()

            ; yield!
                subscription frame model

            ; Raylib.EndDrawing()
            ]

        let rec update model =
            function
            | [] when Raylib.WindowShouldClose() -> Raylib.CloseWindow(); exit 0

            | [] -> update model (rayTick model)

            | (Msg nextMsg) :: remaining ->
                update (nextMsg model) remaining

        init ||> update
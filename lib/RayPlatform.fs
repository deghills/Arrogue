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

    type Msg<'model> = Msg of ('model -> Writer.Writer<Msg<'model>,'model>)

    type IViewable<'model> = interface
        abstract member View : unit -> List<Msg<'model>>
        static member Compose (leftView: #IViewable<'model>) (rightView: #IViewable<'model>) =
            { new IViewable<'model> with member _.View() = leftView.View() @ rightView.View() }
    end
    
    module Msgs =
        let quit<'model>: Msg<'model> =
            let () =
                Raylib.CloseWindow()
                printfn "GAME OVER"
            exit 0

        let changeWindowSize horz vert =
            let () = Raylib.SetWindowSize(horz, vert)
            Writer.return_ |> Msg

    module Viewables =
        let empty<'model> = { new IViewable<'model> with member _.View() = [] }

        type Text<'model> (msg: string, posX: int, posY: int, fontSize: int, ?colour: Color) = class
            new (msg: string, pos: IntVec, fontSize: int, ?colour: Color) =
                Text (msg, pos.X, pos.Y, fontSize, defaultArg colour Raylib.GREEN)

            interface IViewable<'model> with
                member _.View() = let () = Raylib.DrawText(msg, posX, posY, fontSize, defaultArg colour Raylib.GREEN) in []
        end

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

        (*
        Okay
        I think my subscription API will use computation expressions, with custom syntax like 'ontick'
        Should function something like a more domain-specifc IO monad,
        with the custom keywords specifying which platform event to subscribe to

        something like this

        let subscription model =
            computationCE {
                if model.GameplayActive then
                    ontick (fun frame -> [ if frame.KeyPressed(KeyboardKey.W) then yield ... ]
            }
        
        *)

    let run
        (cfg: Config)
        (init: Writer.Writer<Msg<'model>, 'model>)
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
                let (Writer.Writer (intermediate, model')) = nextMsg model
                update model' (intermediate @ remaining)

        match init with
        Writer.Writer (initialMsgs, initialModel) ->
            update initialModel initialMsgs
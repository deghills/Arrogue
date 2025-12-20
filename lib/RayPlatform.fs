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

    type TickInfo internal () =
        let pressedKeys =
            let rec aux acc =
                match Raylib.GetKeyPressed() with
                | 0 ->
                    acc
                | i ->
                    aux (enum<KeyboardKey> i :: acc)
            in aux []
        member _.Frametime = Raylib.GetFrameTime()
        member _.MouseDelta = Raylib.GetMouseDelta()
        member _.MousePosition = Raylib.GetMousePosition()
        member _.Item(k: KeyboardKey) = ButtonInfo k
        member _.Item(m: MouseButton) = ButtonInfo m
        member _.PressedKeys = pressedKeys

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

    module Colours =
        let rayWhite = Raylib.RAYWHITE

    type Msg<'model> = Msg of ('model -> Writer.Writer<Msg<'model>,'model>)
    type Viewable<'model> = View of (Unit -> List<Msg<'model>>)
    type Subscription<'model> = OnTick of (TickInfo -> List<Msg<'model>>)
    
    module PlatformMsgs =
        let quit<'model>: Msg<'model> =
            let () =
                Raylib.CloseWindow()
                printfn "GAME OVER"
            exit 0

        let changeWindowSize horz vert =
            let () = Raylib.SetWindowSize(horz, vert)
            Writer.return_ |> Msg

    module Viewables =

        let compose (View viewLeft) (View viewRight) =
            fun () -> viewLeft() @ viewRight()
            |> View

        let text (msg: string) (pos: IntVec) (fontSize: int) (colour: Color) =
            View (fun () -> let () = Raylib.DrawText(msg, pos.X, pos.Y, fontSize, colour) in [])

        let zero = View (fun () -> [])

    let run
        (cfg: Config)
        (init: Writer.Writer<Msg<'model>, 'model>)
        (view: 'model -> Viewable<'model>)
        (subscription: 'model -> Subscription<'model>)
    
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

            ; yield!
                view model
                |> function View render -> render()

            ; yield!
                subscription model
                |> function OnTick onTick -> TickInfo() |> onTick

            ; Raylib.EndDrawing()
            ]

        let rec update model =
            function
            | [] when Raylib.WindowShouldClose() -> Raylib.CloseWindow(); exit 0

            | [] ->
                update model (rayTick model)

            | (Msg nextMsg) :: remaining ->
                let { Writer.History = intermediate; Writer.Value = model' } = nextMsg model
                update model' (intermediate @ remaining)

        let { Writer.History = initialMsgs; Writer.Value = initialModel } = init
        update initialModel (List.ofSeq initialMsgs)
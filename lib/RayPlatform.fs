namespace Rogue.Lib

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

    type TickInfo() =
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
        ; TextSize: int
        ; BackgroundColour: Color
        ; HideCursor: bool
        ; Fullscreen: bool
        } with
        static member Default =
            { Resolution = 1920, 1080
            ; FPS = 120
            ; TextSize = 16
            ; BackgroundColour = Raylib.BLACK
            ; HideCursor = false
            ; Fullscreen = true
            }

    module Colours =
        let rayWhite = Raylib.RAYWHITE

    [<RequireQualifiedAccess>]
    module View =
        [<Interface>]
        type IViewable<'msg> = abstract member View: Unit -> List<'msg>

        let compose (viewable1: IViewable<'msg>) (viewable2: IViewable<'msg>) =
            { new IViewable<'msg> with member _.View() = viewable1.View() @ viewable2.View() }

        let text (msg: string) (pos: IntVec) (fontSize: int) (colour: Color) =
            { new IViewable<'msg> with
                member _.View() = let () = Raylib.DrawText(msg, pos.X, pos.Y, fontSize, colour) in [] }

        let zero = { new IViewable<'msg> with member _.View() = [] }

    [<RequireQualifiedAccess>]
    module Subscription =
        [<Interface>]
        type ISubscription<'msg> = abstract member OnTick: TickInfo -> List<'msg>

    let quit() = Raylib.CloseWindow()

    let run
        (cfg: Config)
        (init: 'model)
        (view: 'model -> View.IViewable<'msg>)
        (subscription: 'model -> Subscription.ISubscription<'msg>)
        (update: 'model -> 'msg -> ('model * 'msg list))
    
        = do
        Raylib.InitWindow (fst cfg.Resolution, snd cfg.Resolution, "a game to be played")
        Raylib.SetTargetFPS cfg.FPS
        RayGui.GuiSetStyle (int GuiControl.DEFAULT, int GuiDefaultProperty.TEXT_SIZE, cfg.TextSize)
        if cfg.HideCursor then Raylib.HideCursor()
        if cfg.Fullscreen then Raylib.ToggleFullscreen()

        let rec processMsgs msgs state =
            match msgs with
            | nextMsg :: msgQueue ->
                let state', intermediateMsgs = update state nextMsg
                processMsgs (intermediateMsgs @ msgQueue) state'
            | [] -> state
    
        let rec tick state =
            match Raylib.WindowShouldClose() with
            | true -> Raylib.CloseWindow(); exit 0

            | false ->
                Raylib.BeginDrawing()
                Raylib.ClearBackground cfg.BackgroundColour

                let state' =
                    state
                    |> processMsgs (view state |> _.View())
                    |> processMsgs (subscription state |> _.OnTick(TickInfo ()))

                Raylib.EndDrawing()

                tick state'


        in tick init
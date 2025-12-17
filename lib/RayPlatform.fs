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

    type IMsg<'model> = abstract member Update: 'model -> ('model * List<IMsg<'model>>)
    type IViewable<'msg, 'model> = abstract member View: Unit -> List<IMsg<'model>>
    type ISubscription<'msg, 'model> = abstract member OnTick: TickInfo -> List<IMsg<'model>>
    
    module PlatformMsgs =
        let private asMsg f = { new IMsg<'msg> with member _.Update m = f m }

        let quit =
            { new IMsg<'msg> with
                member _.Update _ =
                    let () =
                        Raylib.CloseWindow()
                        printfn "GAME OVER"
                    exit 0
            }

        let changeWindowSize horz vert =
            let () = Raylib.SetWindowSize(horz, vert)
            asMsg (fun model -> model, [])

    module Viewables =

        let compose (viewable1: IViewable<'msg, 'model>) (viewable2: IViewable<'msg, 'model>) =
            { new IViewable<'msg, 'model> with member _.View() = viewable1.View() @ viewable2.View() }

        let text (msg: string) (pos: IntVec) (fontSize: int) (colour: Color) =
            { new IViewable<'msg, 'model> with
                member _.View() = let () = Raylib.DrawText(msg, pos.X, pos.Y, fontSize, colour) in [] }

        let zero = { new IViewable<'msg, 'model> with member _.View() = [] }

    let run
        (cfg: Config)
        (init: 'model)
        (view: 'model -> IViewable<'msg, 'model>)
        (subscription: 'model -> ISubscription<'msg, 'model>)
    
        = do
        Raylib.InitWindow (fst cfg.Resolution, snd cfg.Resolution, "a game to be played")
        Raylib.SetTargetFPS cfg.FPS
        RayGui.GuiSetStyle (int GuiControl.DEFAULT, int GuiDefaultProperty.TEXT_SIZE, cfg.TextSize)
        if cfg.HideCursor then Raylib.HideCursor()
        if cfg.Fullscreen then Raylib.ToggleFullscreen()

        let rec processMsgs (msgs: List<IMsg<'model>>) (state: 'model)=
            match msgs with
            | nextMsg :: msgQueue ->
                let state', intermediateMsgs = nextMsg.Update state
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
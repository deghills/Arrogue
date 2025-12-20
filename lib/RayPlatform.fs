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

        let rec processMsgs (msgs: seq<Msg<'model>>) (state: 'model) =
            match msgs with
            | Seq.Cons (Msg msg, msgQueue) ->
                let { Writer.History = intermediateMsgs; Writer.Value = state' } = msg state
                processMsgs (Seq.append intermediateMsgs msgQueue) state'
            | Seq.Nil -> state
    
        let rec tick state =
            match Raylib.WindowShouldClose() with
            | true -> Raylib.CloseWindow(); exit 0

            | false ->
                Raylib.BeginDrawing()
                Raylib.ClearBackground cfg.BackgroundColour

                let state' =
                    state
                    |> processMsgs (view state |> function View render -> render())
                    |> processMsgs (subscription state |> function OnTick f -> (f << TickInfo)() )

                Raylib.EndDrawing()

                tick state'
        let { Writer.History = initialMsgs; Writer.Value = initialModel } = init in initialModel |> processMsgs initialMsgs |> tick
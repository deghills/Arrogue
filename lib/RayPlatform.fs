namespace Rogue.Lib

module RayPlatform =

    open System
    open System.Numerics
    open ZeroElectric.Vinculum

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
        member this.WASD =
            let float32 = function true -> 1f | false -> 0f
            ( float32 this[KeyboardKey.KEY_D].IsDown - float32 this[KeyboardKey.KEY_A].IsDown
            , float32 this[KeyboardKey.KEY_S].IsDown - float32 this[KeyboardKey.KEY_W].IsDown )
            |> Vector2
            |> Vector2.Normalize

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
            ; BackgroundColour = Raylib.BEIGE
            ; HideCursor = false
            ; Fullscreen = true
            }

    type MsgQueue<'msg>() =
        let queue = Collections.Concurrent.ConcurrentQueue<'msg>()
        let mutable current = None
        let _enumerator() =
            { new Collections.Generic.IEnumerator<'msg> with
                member _.Current: 'msg = match current with Some msg -> msg | None -> failwith "AHHH"
                member this.Current: obj = this.Current
                member _.MoveNext () =
                    match queue.TryDequeue() with
                    | false, _ -> false
                    | true, msg -> let () = current <- Some msg in true
                member _.Reset () = current <- None
                member _.Dispose() = ()
            }

        member _.Dispatch = queue.Enqueue
        member _.Pop(): 'msg option =
            match queue.TryDequeue() with
            | true, msg -> Some msg
            | false, _ -> None
    
        interface seq<'msg> with
            member _.GetEnumerator (): Collections.Generic.IEnumerator<'msg> = 
                _enumerator()

            member _.GetEnumerator (): Collections.IEnumerator = 
                _enumerator()

    let run
        (cfg: Config)
        (init: 'model)
        (view: 'model -> unit)
        (update: 'model -> 'msg -> ('model * 'msg list))
        (onTick: TickInfo -> 'model -> 'msg list)
    
        = do
        Raylib.InitWindow (fst cfg.Resolution, snd cfg.Resolution, "a game to be played")
        Raylib.SetTargetFPS cfg.FPS
        RayGui.GuiSetStyle (int GuiControl.DEFAULT, int GuiDefaultProperty.TEXT_SIZE, cfg.TextSize)
        if cfg.HideCursor then Raylib.HideCursor()
        if cfg.Fullscreen then Raylib.ToggleFullscreen()

        let rec processMsgs (msgs: 'msg list) (state: 'model) =
            match msgs with
            | nextMsg :: remainingMsgs ->
                let state', intermediateMsgs = update state nextMsg
                processMsgs (intermediateMsgs @ remainingMsgs) state'
            | [] ->
                state
    
        let rec tick model (msgQueue: 'msg list) =
            match Raylib.WindowShouldClose() with
            | true ->
                Raylib.CloseWindow()
            | false ->
                Raylib.BeginDrawing()
                Raylib.ClearBackground cfg.BackgroundColour

                view model
            
                let model' = processMsgs msgQueue model
                let msgQueue' = onTick (TickInfo()) model

                Raylib.EndDrawing()

                tick model' msgQueue'
        in tick init []
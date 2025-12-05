open System
open Rogue.Lib
open ProjectUtils
open RayPlatform
open Creature
open Model
open Update
open View
open Subscription
open BSP

let init =
    { Creatures =
        Map
            [ CreatureID.player, { Pos = IntVec.Vec (20, 20); Token = '@'; Stats = { Health = 100; Strength = 10 } }
            ; enum<CreatureID> 0, { Pos = IntVec.Vec (15, 15); Token = 'g'; Stats = { Health = 100; Strength = 11 } } ]
    ; Walls =
        Set [ for j in 0..20 do if j <> 13 && j <> 0 then yield Vec (17, j) ]
    }

(*RayPlatform.run
    { RayPlatform.Config.Default with Fullscreen = false }
    init
    view
    update
    subscriptions*)

open ZeroElectric.Vinculum

let drawPart (bounds: BSP.Bounds.t) =
    let (posX, posY) = (bounds.MinX, bounds.MinY)
    let width = bounds.MaxX - bounds.MinX
    let height = bounds.MaxY - bounds.MinY
    Raylib.DrawRectangleLines
        ( posX * cellSize
        , posY * cellSize
        , width * cellSize
        , height * cellSize
        , Raylib.RED
        )

let partitions =
    Bounds.t (0, 64, 0, 32)
    |> SplitTree.Leaf
    |> BSP.splitRandom 4 5
    |> SplitTree.map
        (fun bounds -> Bounds.t (bounds.MinX + 1, bounds.MaxX - 1, bounds.MinY + 1, bounds.MaxY - 1))
    |> buildRandomPaths

Raylib.InitWindow(1920, 1080, "caucuses and bollocks")

while not (Raylib.WindowShouldClose()) do
    Raylib.BeginDrawing()
    Raylib.ClearBackground Raylib.BLACK
    
    //for part in partitions do drawPart part
    for wall in BSP.toWalls partitions do drawChar wall '='

    Raylib.EndDrawing()
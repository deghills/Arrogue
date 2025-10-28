namespace Rogue.Lib.Game

open System
open Rogue.Lib

[<CustomComparison; CustomEquality>]
type TileKey =
    { ID: CreatureID; Pos: IntVec }

    override this.Equals(that: obj) =
        match that with
        | :? TileKey as that ->
            this.ID = that.ID || this.Pos = that.Pos
        | _ -> raise (ArgumentException())

    interface IComparable<TileKey> with
        member this.CompareTo (that: TileKey): int = 
            match int this.ID - int that.ID with
            | _ when this.Pos = that.Pos ->
                0
            | comparison ->
                comparison

    interface IComparable with
        member this.CompareTo (obj: obj): int =
            match obj with
            | :? TileKey as that -> (this :> IComparable<TileKey>).CompareTo that
            | _ -> raise (System.ArgumentException())

type Tile =
    | Creature of Creature
    | Wall

type TileMap =
    val private tiles: Map<TileKey, Tile>
    new (tiles) = { tiles = tiles }
    new () = { tiles = Map.empty }
    new (tileSeq) = TileMap (Map tileSeq)

    static member Add key value (tileMap: TileMap) =
        tileMap.tiles
        |> Map.add key value
        |> TileMap

    member this.Item key = Map.tryFind key this.tiles
    
    member this.Item (ID: CreatureID) =
        Map.toSeq this.tiles
        |> Seq.tryFind
            (function
            | { ID = id' }, _ when ID = id' -> true
            | _ -> false)
        |> Option.map snd

    member this.Item (pos: IntVec) =
        Map.toSeq this.tiles
        |> Seq.tryFind
            (function
            | { Pos = pos' }, _ when pos = pos' -> true
            | _ -> false)
        |> Option.map snd

    member this.Contains (key: TileKey) =
        this.tiles.ContainsKey key

    member this.Contains (ID: CreatureID) =
        this.tiles.Keys
        |> Seq.exists (function { ID = id' } when ID = id' -> true | _ -> false)

    member this.Contains (pos: IntVec) =
        this.tiles.Keys
        |> Seq.exists (function { Pos = pos' } when pos = pos' -> true | _ -> false)

    member this.TrySpawn (creature: Creature, pos: IntVec, ?ID: CreatureID) =
        defaultArg
            ID
            (Seq.initInfinite enum<CreatureID> |> Seq.find (this.Contains >> not))
        |> fun validID -> TileMap.Add ({ ID = validID; Pos = pos }) (Creature creature) this

    interface seq<TileKey*Tile> with
        member this.GetEnumerator (): Collections.Generic.IEnumerator<TileKey * Tile> = 
            (Map.toSeq this.tiles).GetEnumerator()

        member this.GetEnumerator (): Collections.IEnumerator = 
            (this :> seq<TileKey*Tile>).GetEnumerator()
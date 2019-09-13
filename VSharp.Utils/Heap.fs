namespace VSharp

open System.Collections
open System.Collections.Generic
open FSharpx.Collections

[<CustomEquality;NoComparison>]
type public commonHeap<'key, 'term, 'fql, 'typ> when 'key : equality and 'term : equality =
    {heap : PersistentHashMap<commonMemoryCell<'key, 'fql, 'typ>, 'term>}
    static member Empty() = {heap = PersistentHashMap<commonMemoryCell<'a, 'fql, 'typ>, 'b>.Empty()}
    member x.Length = x.heap.Length
    member x.ContainsKey(key) = x.heap.ContainsKey({key = key; FQL = None; typ = Unchecked.defaultof<'typ>})
    member x.ContainsKey(key) = x.heap.ContainsKey(key)
    member x.Add(pair) = {heap = x.heap.Add(pair)}
    member x.Remove(key) = x.heap.Remove(key)
    member x.Item
        with get key = x.heap.[{key = key; FQL = None; typ = Unchecked.defaultof<'typ>}]
    member x.Item
        with get key = x.heap.[key]
    static member ofSeq(items) = {heap = PersistentHashMap<commonMemoryCell<'key, 'fql, 'typ>, 'term>.ofSeq(items)}
    member x.Iterator() = x.heap.Iterator()

    interface IEnumerable<commonMemoryCell<'key, 'fql, 'typ> * 'term> with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator()

    interface IEnumerable with
        member x.GetEnumerator () =
          x.Iterator().GetEnumerator() :> IEnumerator

    override x.GetHashCode() = x.heap :> seq<commonMemoryCell<'key, 'fql, 'typ> * 'term> |> List.ofSeq |> fun l -> l.GetHashCode()

    override x.Equals(o : obj) =
        match o with
        | :? commonHeap<'key, 'term, 'fql, 'typ> as h -> x.GetHashCode() = h.GetHashCode()
        | _ -> false

module public Heap =

    let public getKey key = key.key

    let public empty<'a, 'b, 'fql, 'typ when 'a : equality and 'b : equality> : commonHeap<'a, 'b, 'fql, 'typ> = commonHeap<'a, 'b, 'fql, 'typ>.Empty()
    let public isEmpty h = PersistentHashMap.length h.heap = 0

    let public ofSeq = commonHeap<'a, 'b, 'fql, 'typ>.ofSeq
    let public toSeq (h : commonHeap<'a, 'b, 'fql, 'typ>) = h :> seq<commonMemoryCell<'a, 'fql, 'typ> * 'b>

    let public contains (key : 'a) (h : commonHeap<'a, 'b, 'fql, 'typ>) = h.ContainsKey key
    let public containsKey (key : commonMemoryCell<'a, 'fql, 'typ>) (h : commonHeap<'a, 'b, 'fql, 'typ>) = h.ContainsKey key
    let public find (key : 'a) (h : commonHeap<'a, 'b, 'fql, 'typ>) = h.[key]
    let public findKey (key : commonMemoryCell<'a, 'fql, 'typ>) (h : commonHeap<'a, 'b, 'fql, 'typ>) = h.[key]
    let public add key value (h : commonHeap<'a, 'b, 'fql, 'typ>) = h.Add(key, value)

    let public size (h : commonHeap<'a, 'b, 'fql, 'typ>) = h.Length

    let private mapKeyValue mapper (key, value) =
        let k, v = mapper key.key value
        ({key with key = k}, v)

    let public map mapper (h : commonHeap<'a, 'b, 'fql, 'typ>) : commonHeap<'a, 'c, 'fql, 'typ> =
        h |> toSeq |> Seq.map (mapKeyValue mapper) |> ofSeq
    let public map' mapper (h : commonHeap<'a, 'b, 'fql, 'typ>) : commonHeap<'a, 'c, 'fql, 'typ> =
        h |> toSeq |> Seq.map (fun (k, v) -> k, mapper k.key v) |> ofSeq
    let public foldFQL folder state (h : commonHeap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.fold (fun state (k, v) -> folder state k v) state
    let public fold folder state (h : commonHeap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.fold (fun state (k, v) -> folder state k.key v) state

    let private mapFoldKeyValue folder state (key, value) =
        let (k, v), state = folder state key.key value
        ({key with key = k}, v), state

    let public mapFold folder state (h : commonHeap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.mapFold (mapFoldKeyValue folder) state |> mapfst ofSeq

    let private fqlLocations (h : commonHeap<'a, 'b, 'fql, 'typ>) = h |> toSeq |> Seq.map fst
    let public locations (h : commonHeap<'a, 'b, 'fql, 'typ>) = h |> toSeq |> Seq.map (getKey << fst)
    let public values (h : commonHeap<'a, 'b, 'fql, 'typ>) = h |> toSeq |> Seq.map (fun (_, v) -> v)

    let public partition predicate (h : commonHeap<'a, 'b, 'fql, 'typ>) =
        h |> toSeq |> Seq.map (fun (k, v) -> (k.key, v)) |> List.ofSeq |> List.partition predicate

    let public unify acc guards (heaps : commonHeap<'a, 'b, 'fql, 'typ> list) unifier =
        let keys = new System.Collections.Generic.HashSet<commonMemoryCell<'a, 'fql, 'typ>>()

        List.iter (fqlLocations >> keys.UnionWith) heaps
        let unifyOneKey acc k =
            let hgvs = List.mapi2 (fun i g h -> if containsKey k h then (i, g, Some(h.[k])) else (i, g, None)) guards heaps
            unifier acc k hgvs
        Seq.fold unifyOneKey acc keys

    let public merge guards (heaps : commonHeap<'a, 'b, 'fql, 'typ> list) resolve =
        unify heaps.Head guards heaps (fun acc k hgvs -> add k (resolve k hgvs) acc)

    let public unify2 acc (h1 : commonHeap<'a, 'b, 'fql, 'typ>) (h2 : commonHeap<'a, 'b, 'fql, 'typ>) unifier =
        let keysSet = HashSet(fqlLocations h1)
        keysSet.UnionWith(fqlLocations h2)
        let unifyIfShould acc key =
            match containsKey key h1, containsKey key h2 with
            | true, true  -> unifier acc key (Some h1.[key]) (Some h2.[key])
            | true, false -> unifier acc key (Some h1.[key]) None
            | false, true -> unifier acc key None (Some h2.[key])
            | _ -> __unreachable__()
        Seq.fold unifyIfShould acc keysSet

    let public merge2 (h1 : commonHeap<'a, 'b, 'fql, 'typ>) (h2 : commonHeap<'a, 'b, 'fql, 'typ>) resolve =
        unify2 h1 h1 h2 (fun s k v1 v2 -> add k (resolve k v1 v2) s)

    let public toString format separator keyMapper valueMapper sorter (h : commonHeap<'a, 'b, 'fql, 'typ>) =
        let elements =
            h
            |> toSeq
            |> Seq.map (fun (k, v) -> k.key, v)
            |> Seq.sortBy sorter
            |> Seq.map (fun (k, v) -> sprintf format (keyMapper k) (valueMapper v))
        elements |> join separator

    let public dump (h : commonHeap<'a, 'b, 'fql, 'typ>) keyToString = toString "%s ==> %O" "\n" keyToString id Prelude.toString h

// From https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184
namespace WoofWare.WeakHashTable

open System
open System.Collections.Generic
open System.Collections.Concurrent
open System.Runtime.CompilerServices

/// <summary>
/// A hashtable that keeps a weak pointer to each key's data and uses a finalizer to
/// detect when the data is no longer referenced (by any non-weak pointers).
/// </summary>
/// <remarks>
///    Once a key's data is finalized, the table will effectively behave as if the key is not
///    in the table, e.g., [find] will return [None]. However, one must call
///    [reclaim_space_for_keys_with_unused_data] to actually reclaim the space used by the
///    table for such keys.
///
///    Unlike (OCaml's) [Weak.Make], which also describes itself as a "weak hashtable,"
///    [Weak_hashtbl] gives a dictionary-style structure. In fact, OCaml's [Weak.Make] may
///    better be described as a weak set.
/// </remarks>
/// <remarks>
///    There's a tricky type of bug one can write with this module, e.g.:
///
///    {[
///      type t =
///        { foo : string
///        ; bar : float Incr.t
///        }
///
///      let tbl = Weak_hashtbl.create ()
///      let x1 =
///        let t = Weak_hashtbl.find_or_add tbl key ~default:(fun () ->
///          (... some function that computes a t...))
///        in
///        t.bar
///    ]}
///
///    At this point, the data associated with [key] is unreachable (since all we did with it
///    was project out field [bar]), so it may disappear from the table at any time. *)
/// </remarks>
type WeakHashTable<'Key, 'Value when 'Key : equality and 'Value : not struct> =
    private
        {
            EntryByKey : Dictionary<'Key, WeakReference>
            KeysWithUnusedData : ConcurrentQueue<'Key>
            Sentinels : ConditionalWeakTable<'Value, Sentinel<'Key, 'Value>>
            mutable ThreadSafeRunWhenUnusedData : unit -> unit
        }

/// Tracks keys that reference a particular value so they can all be enqueued when that value dies.
and private Sentinel<'Key, 'Value when 'Key : equality and 'Value : not struct> (table : WeakHashTable<'Key, 'Value>) =
    let keys = ConcurrentQueue<'Key> ()

    member _.AddKey key = keys.Enqueue key

    override _.Finalize () =
        try
            let mutable enqueued = false

            let mutable continueLooping = true

            while continueLooping do
                let mutable key = Unchecked.defaultof<'Key>

                match keys.TryDequeue &key with
                | true ->
                    table.KeysWithUnusedData.Enqueue key
                    enqueued <- true
                | false -> continueLooping <- false

            if enqueued then
                table.ThreadSafeRunWhenUnusedData ()
        with _ ->
            ()

/// Visitor for a WeakHashTable, intended for use with WeakHashTableCrate.
type WeakHashTableEval<'ret> =
    /// Specifies the action to be taken when this visitor is applied to a WeakHashTableCrate.
    abstract Eval<'a, 'b when 'a : equality and 'b : not struct> : WeakHashTable<'a, 'b> -> 'ret

/// A WeakHashTable with its type parameters hidden. Visit it with a WeakHashTableEval.
type WeakHashTableCrate =
    /// Apply the given visitor to the WeakHashTable contained within this WeakHashTableCrate.
    abstract Apply<'ret> : WeakHashTableEval<'ret> -> 'ret

/// A WeakHashTable with its type parameters hidden. Visit it with a WeakHashTableEval.
[<RequireQualifiedAccess>]
module WeakHashTableCrate =
    /// Hide the type parameter of the input.
    let make<'k, 'v when 'k : equality and 'v : not struct> (c : WeakHashTable<'k, 'v>) : WeakHashTableCrate =
        { new WeakHashTableCrate with
            member _.Apply e = e.Eval c
        }

[<RequireQualifiedAccess>]
module WeakHashTable =
    /// Creates a new weak hash table
    let create<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (initialCapacity : int option)
        : WeakHashTable<'Key, 'Value>
        =
        {
            EntryByKey = Dictionary<'Key, WeakReference> (defaultArg initialCapacity 0)
            KeysWithUnusedData = ConcurrentQueue<'Key> ()
            Sentinels = ConditionalWeakTable<'Value, Sentinel<'Key, 'Value>> ()
            ThreadSafeRunWhenUnusedData = ignore
        }

    /// Sets the callback to run when unused data is detected
    let setRunWhenUnusedData<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (threadSafeF : unit -> unit)
        : unit
        =
        t.ThreadSafeRunWhenUnusedData <- threadSafeF

    /// Removes a key from the table
    let remove<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        : unit
        =
        t.EntryByKey.Remove key |> ignore<bool>

    /// Clears all entries from the table
    let clear<'Key, 'Value when 'Key : equality and 'Value : not struct> (t : WeakHashTable<'Key, 'Value>) : unit =
        t.EntryByKey.Clear ()

    /// Reclaims space for keys whose values have been garbage collected
    let reclaimSpaceForKeysWithUnusedData<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        : unit
        =
        let rec processQueue () =
            match t.KeysWithUnusedData.TryDequeue () with
            | true, key ->
                match t.EntryByKey.TryGetValue key with
                | true, entry when not entry.IsAlive -> remove t key
                | _ -> ()

                processQueue ()
            | false, _ -> ()

        processQueue ()

    /// Gets or creates a weak reference entry for a key
    let private getEntry<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        : WeakReference
        =
        match t.EntryByKey.TryGetValue key with
        | true, entry -> entry
        | false, _ ->
            let entry = WeakReference (null : obj)
            t.EntryByKey.[key] <- entry
            entry

    /// Checks if a key exists with a live value
    let mem<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        : bool
        =
        match t.EntryByKey.TryGetValue key with
        | true, entry -> entry.IsAlive
        | false, _ -> false

    /// Checks if a key is using space in the table (regardless of whether value is alive)
    let keyIsUsingSpace<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        : bool
        =
        t.EntryByKey.ContainsKey key

    /// Sets data for an entry and registers finalizer
    let private setData<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (entry : WeakReference)
        (data : 'Value)
        : unit
        =
        entry.Target <- data

        // Register a finalizer to enqueue the key when the value is collected
        let sentinel =
            t.Sentinels.GetValue (
                data,
                ConditionalWeakTable<'Value, Sentinel<'Key, 'Value>>.CreateValueCallback (fun _ ->
                    new Sentinel<'Key, 'Value> (t)
                )
            )

        sentinel.AddKey key

    /// Replaces the value for a key
    let replace<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (data : 'Value)
        : unit
        =
        setData t key (getEntry t key) data

    /// Adds a new key-value pair, raising an exception if key already exists with live value
    let addThrowing<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (data : 'Value)
        : unit
        =
        let entry = getEntry t key

        if entry.IsAlive then
            failwithf "WeakHashTable.addThrowing: key already in use"

        setData t key entry data

    /// Finds the value associated with a key
    let find<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        : 'Value option
        =
        match t.EntryByKey.TryGetValue key with
        | true, entry ->
            match entry.Target with
            | :? 'Value as value -> Some value
            | _ -> None
        | false, _ -> None

    /// Finds the value for a key or adds a new one using the default function
    let findOrAdd<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (defaultF : unit -> 'Value)
        : 'Value
        =
        let entry = getEntry t key

        match entry.Target with
        | :? 'Value as value -> value
        | _ ->
            let data = defaultF ()
            setData t key entry data
            data

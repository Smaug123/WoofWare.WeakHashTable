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
            /// Non-null values stored with weak references
            EntryByKey : Dictionary<'Key, WeakReference>
            /// Keys explicitly set to null (no cleanup needed - null can't be collected)
            NullValueKeys : HashSet<'Key>
            KeysWithUnusedData : ConcurrentQueue<'Key>
            mutable ThreadSafeRunWhenUnusedData : unit -> unit
            /// Weak table that ties finalizer objects to values. Must be rooted to prevent premature collection.
            CleanupTable : ConditionalWeakTable<'Value, Object>
        }

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
            NullValueKeys = HashSet<'Key> ()
            KeysWithUnusedData = ConcurrentQueue<'Key> ()
            ThreadSafeRunWhenUnusedData = ignore
            CleanupTable = ConditionalWeakTable<'Value, Object> ()
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
        t.NullValueKeys.Remove key |> ignore<bool>

    /// Clears all entries from the table
    let clear<'Key, 'Value when 'Key : equality and 'Value : not struct> (t : WeakHashTable<'Key, 'Value>) : unit =
        t.EntryByKey.Clear ()
        t.NullValueKeys.Clear ()

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
        t.NullValueKeys.Contains key
        || match t.EntryByKey.TryGetValue key with
           | true, entry -> entry.IsAlive
           | false, _ -> false

    /// Checks if a key is using space in the table (regardless of whether value is alive)
    let keyIsUsingSpace<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        : bool
        =
        t.NullValueKeys.Contains key || t.EntryByKey.ContainsKey key

    /// Sets data for an entry and registers finalizer
    let private setData<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (entry : WeakReference)
        (data : 'Value)
        : unit
        =
        entry.Target <- data

        // Register a finalizer to enqueue the key when the value is collected.
        // The callbackObj is stored in t.CleanupTable (rooted) with data as key.
        // When data becomes unreachable, the CWT releases callbackObj, triggering its finalizer.
        let callbackObj =
            { new Object() with
                override _.Finalize () =
                    try
                        t.KeysWithUnusedData.Enqueue key
                        t.ThreadSafeRunWhenUnusedData ()
                    with _ ->
                        ()
            }

        t.CleanupTable.Add (data, callbackObj)

    /// Replaces the value for a key
    let replace<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (data : 'Value)
        : unit
        =
        if Object.isNull data then
            // Null value: store in NullValueKeys, remove from EntryByKey
            t.NullValueKeys.Add key |> ignore<bool>
            t.EntryByKey.Remove key |> ignore<bool>
        else
            // Non-null value: store in EntryByKey, remove from NullValueKeys
            t.NullValueKeys.Remove key |> ignore<bool>
            setData t key (getEntry t key) data

    /// Adds a new key-value pair, raising an exception if key already exists with live value
    let addThrowing<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (data : 'Value)
        : unit
        =
        // Check if key already has a live value (either null or non-null)
        if t.NullValueKeys.Contains key then
            failwithf "WeakHashTable.addThrowing: key already in use"

        if Object.isNull data then
            // Null value: just add to NullValueKeys
            t.NullValueKeys.Add key |> ignore<bool>
        else
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
        if t.NullValueKeys.Contains key then
            Some Unchecked.defaultof<'Value>
        else
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
        // First check if key has a null value
        if t.NullValueKeys.Contains key then
            Unchecked.defaultof<'Value>
        else
            match t.EntryByKey.TryGetValue key with
            | true, entry ->
                match entry.Target with
                | :? 'Value as value -> value
                | _ ->
                    // Entry exists but value is dead; get new value
                    let data = defaultF ()

                    if Object.isNull data then
                        // Store null in NullValueKeys, remove dead entry from EntryByKey
                        t.NullValueKeys.Add key |> ignore<bool>
                        t.EntryByKey.Remove key |> ignore<bool>
                    else
                        setData t key entry data

                    data
            | false, _ ->
                // No entry exists; get value first
                let data = defaultF ()

                if Object.isNull data then
                    // Store null in NullValueKeys
                    t.NullValueKeys.Add key |> ignore<bool>
                else
                    let entry = WeakReference (null : obj)
                    t.EntryByKey.[key] <- entry
                    setData t key entry data

                data

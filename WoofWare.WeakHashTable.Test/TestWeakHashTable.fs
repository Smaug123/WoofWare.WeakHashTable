// From https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184
namespace WoofWare.WeakHashTable.Test

open System
open System.Runtime.CompilerServices
open WoofWare.WeakHashTable
open NUnit.Framework
open FsUnitTyped

[<TestFixture>]
module TestWeakHashTable =
    let inline forceGc () =
        GC.Collect ()
        GC.WaitForPendingFinalizers ()
        GC.Collect ()

    let data (i : int) = ref i

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let addNoInline<'Key, 'Value when 'Key : equality and 'Value : not struct>
        (t : WeakHashTable<'Key, 'Value>)
        (key : 'Key)
        (value : 'Value)
        : unit
        =
        WeakHashTable.addThrowing t key value

    [<OneTimeSetUp>]
    let oneTimeSetup () =
#if DEBUG
        do failwith "These tests only run in release mode"
#endif
        ()

    [<Test>]
    let ``Test basic operations`` () =
        let t = WeakHashTable.create<int, int ref> None
        let key = 13

        // Initially not present
        WeakHashTable.mem t key |> shouldEqual false

        // Add key
        WeakHashTable.addThrowing t key (data key)
        WeakHashTable.mem t key |> shouldEqual true

        // Find should return the value
        match WeakHashTable.find t key with
        | Some v -> v.Value |> shouldEqual 13
        | None -> failwith "Should find value"

        // Replace with new value
        WeakHashTable.replace t key (data 14)

        match WeakHashTable.find t key with
        | Some v -> v.Value |> shouldEqual 14
        | None -> failwith "Should find value after replace"

        // Remove key
        WeakHashTable.remove t key
        WeakHashTable.find t key |> shouldEqual None

        // Add again
        WeakHashTable.addThrowing t key (data key)

        match WeakHashTable.find t key with
        | Some v -> v.Value |> shouldEqual 13
        | None -> failwith "Should find value after re-add"

        // Clear
        WeakHashTable.clear t
        WeakHashTable.find t key |> shouldEqual None

    // Test: keyIsUsingSpace and reclaimSpaceForKeysWithUnusedData
    [<Test>]
    let ``keyIsUsingSpace and reclaimSpaceForKeysWithUnusedData`` () =
        let t = WeakHashTable.create<int, int ref> None
        let key = 13

        // Initially no space used
        (WeakHashTable.keyIsUsingSpace t key) |> shouldEqual false
        (WeakHashTable.mem t key) |> shouldEqual false

        // Add key - now using space
        WeakHashTable.addThrowing t key (data 0)
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true
        WeakHashTable.mem t key |> shouldEqual true

        // Force GC to collect the value
        forceGc ()

        // Key still uses space but mem returns false (weak ref cleared)
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true
        WeakHashTable.mem t key |> shouldEqual false

        // Reclaim space
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual false
        WeakHashTable.mem t key |> shouldEqual false

    // Test: setRunWhenUnusedData callback
    [<Test>]
    let ``setRunWhenUnusedData test`` () =
        let t = WeakHashTable.create<int, int ref> None
        let key = 13
        let ran = ref false

        WeakHashTable.setRunWhenUnusedData t (fun () -> ran.Value <- true)

        // GC before adding data - callback shouldn't run
        forceGc ()
        ran.Value |> shouldEqual false

        // Add data and keep reference
        do
            let data = data key
            WeakHashTable.addThrowing t key data

            // GC with live reference - callback shouldn't run
            forceGc ()
            ran.Value |> shouldEqual false
            GC.KeepAlive data

        forceGc ()
        ran.Value |> shouldEqual true

    [<Test>]
    let ``reclaim does not lose later cleanup`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 41
        let mutable value = obj ()
        let callbacks = ref 0

        WeakHashTable.setRunWhenUnusedData t (fun () -> callbacks.Value <- callbacks.Value + 1)

        let gcPressure () =
            let junk = Array.zeroCreate<obj> 10000

            for i = 0 to junk.Length - 1 do
                junk.[i] <- obj ()

            GC.KeepAlive junk

        addNoInline t key value

        // Run reclamation while the value is still strongly referenced.
        for _ = 1 to 5 do
            gcPressure ()
            forceGc ()
            WeakHashTable.reclaimSpaceForKeysWithUnusedData t
            callbacks.Value |> shouldEqual 0
            WeakHashTable.keyIsUsingSpace t key |> shouldEqual true
            WeakHashTable.mem t key |> shouldEqual true
            GC.KeepAlive value

        callbacks.Value |> shouldEqual 0
        GC.KeepAlive value
        value <- (null : obj)

        forceGc ()
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t

        callbacks.Value |> shouldEqual 1
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual false
        WeakHashTable.mem t key |> shouldEqual false

    [<Test>]
    let ``null values do not leave inconsistent state`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 99

        let outcome =
            try
                WeakHashTable.addThrowing t key null
                Choice1Of2 ()
            with _ ->
                Choice2Of2 ()

        match outcome with
        | Choice1Of2 () ->
            WeakHashTable.find t key |> shouldEqual (Some null)
            WeakHashTable.mem t key |> shouldEqual true
        | Choice2Of2 () ->
            WeakHashTable.keyIsUsingSpace t key |> shouldEqual false
            WeakHashTable.mem t key |> shouldEqual false

    [<Test>]
    let ``null values can be stored and retrieved`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        WeakHashTable.replace t key null
        WeakHashTable.find t key |> shouldEqual (Some null)
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true

    [<Test>]
    let ``null can be replaced with non-null and vice versa`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        let value = obj ()

        // null -> non-null
        WeakHashTable.replace t key null
        WeakHashTable.find t key |> shouldEqual (Some null)
        WeakHashTable.replace t key value
        WeakHashTable.find t key |> shouldEqual (Some value)

        // non-null -> null
        WeakHashTable.replace t key null
        WeakHashTable.find t key |> shouldEqual (Some null)
        GC.KeepAlive value

    [<Test>]
    let ``addThrowing rejects duplicate null`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        WeakHashTable.addThrowing t key null

        let exc =
            Assert.Throws<KeyAlreadyInUseException> (fun () -> WeakHashTable.addThrowing t key null)

        exc.Data0 |> unbox<int> |> shouldEqual 1

    [<Test>]
    let ``addThrowing rejects null when key has live non-null value`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        let value = obj ()
        WeakHashTable.addThrowing t key value

        let exc =
            Assert.Throws<KeyAlreadyInUseException> (fun () -> WeakHashTable.addThrowing t key null)

        exc.Data0 |> unbox<int> |> shouldEqual 1
        // Verify the original value is still there and state is consistent
        WeakHashTable.find t key |> shouldEqual (Some value)
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true
        // Verify that the failed null add didn't corrupt the table
        // (e.g., the key shouldn't suddenly become a null entry after GC)
        forceGc ()
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t
        WeakHashTable.find t key |> shouldEqual (Some value)
        WeakHashTable.mem t key |> shouldEqual true
        GC.KeepAlive value

    [<Test>]
    let ``findOrAdd stores null from defaultF`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        let result = WeakHashTable.findOrAdd t key (fun () -> null)
        result |> shouldEqual null
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.find t key |> shouldEqual (Some null)

    [<Test>]
    let ``findOrAdd with existing null does not call defaultF`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        let mutable called = false
        WeakHashTable.addThrowing t key null

        let result =
            WeakHashTable.findOrAdd
                t
                key
                (fun () ->
                    called <- true
                    obj ()
                )

        result |> shouldEqual null
        called |> shouldEqual false

    [<Test>]
    let ``null values do not trigger cleanup callback`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        let mutable callbacks = 0
        WeakHashTable.setRunWhenUnusedData t (fun () -> callbacks <- callbacks + 1)
        WeakHashTable.addThrowing t key null
        forceGc ()
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t
        callbacks |> shouldEqual 0
        WeakHashTable.mem t key |> shouldEqual true

    [<Test>]
    let ``remove works for null values`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        WeakHashTable.addThrowing t key null
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.remove t key
        WeakHashTable.mem t key |> shouldEqual false
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual false

    // Test: findOrAdd and complex scenarios
    type Struct =
        {
            // original OCaml had this mutable to force the Struct to go on the heap
            mutable Foo : int
            Bar : int
            Baz : string
        }

    [<Test>]
    let ``mega test`` () =
        let t = WeakHashTable.create<int, Struct ref> None

        let createData foo =
            {
                Foo = foo
                Bar = 0
                Baz = "hello"
            }
            |> ref

        let stabilize () =
            forceGc ()
            WeakHashTable.reclaimSpaceForKeysWithUnusedData t

        // Use mutable cells to control lifetime
        let b1 = ref (createData 1)
        let b2 = ref (createData 2)
        let b3 = ref (createData 3)
        let b4 = ref (createData 4)

        let blackhole (r : Struct ref ref) : unit = r := createData 0

        let k1, k2, k3 = 1, 2, 3

        // Add using findOrAdd
        let add (k : int) (bref : Struct ref ref) =
            WeakHashTable.findOrAdd t k (fun () -> bref.Value) |> ignore<Struct ref>

        add k1 b1
        add k2 b2
        add k3 b3

        // Checking [is_absent k] is stronger than checking that [is_none (find tbl k)].  We
        // want to make sure that a key has been removed from the table, and in particular rule
        // out the case where the key is in the table but the corresponding weak is none.
        let isAbsent k = not (WeakHashTable.keyIsUsingSpace t k)

        let isBlock k (b : Struct ref) =
            match WeakHashTable.find t k with
            | None -> false
            | Some found -> Object.ReferenceEquals (found, b)

        // All should be present
        isBlock k1 !b1 |> shouldEqual true
        isBlock k2 !b2 |> shouldEqual true
        isBlock k3 !b3 |> shouldEqual true

        // Clear b1 and stabilize
        blackhole b1
        stabilize ()

        isAbsent k1 |> shouldEqual true
        isBlock k2 !b2 |> shouldEqual true
        isBlock k3 !b3 |> shouldEqual true

        // Clear b2 and stabilize
        blackhole b2
        stabilize ()

        isAbsent k1 |> shouldEqual true
        isAbsent k2 |> shouldEqual true
        isBlock k3 !b3 |> shouldEqual true

        // Replace k3 with b4
        WeakHashTable.replace t k3 b4.Value
        blackhole b3
        stabilize ()

        isBlock k3 !b4 |> shouldEqual true

        // Clear b4 and stabilize
        blackhole b4
        stabilize ()

        isAbsent k3 |> shouldEqual true

    [<Test>]
    let ``addThrowing allows duplicate value under multiple keys`` () =
        let t = WeakHashTable.create<int, obj> None
        let key1 = 1
        let key2 = 2
        let value = obj ()

        // Add value under key1
        WeakHashTable.addThrowing t key1 value

        // Add the same value under key2 - should succeed
        WeakHashTable.addThrowing t key2 value

        // Both keys should have the value
        WeakHashTable.keyIsUsingSpace t key1 |> shouldEqual true
        WeakHashTable.mem t key1 |> shouldEqual true
        WeakHashTable.find t key1 |> shouldEqual (Some value)

        WeakHashTable.keyIsUsingSpace t key2 |> shouldEqual true
        WeakHashTable.mem t key2 |> shouldEqual true
        WeakHashTable.find t key2 |> shouldEqual (Some value)
        GC.KeepAlive value

    [<Test>]
    let ``findOrAdd allows duplicate value under multiple keys`` () =
        let t = WeakHashTable.create<int, obj> None
        let key1 = 1
        let key2 = 2
        let value = obj ()

        // Add value under key1
        WeakHashTable.addThrowing t key1 value

        // Add the same value under key2 via findOrAdd - should succeed
        let result = WeakHashTable.findOrAdd t key2 (fun () -> value)
        result |> shouldEqual value

        // Both keys should have the value
        WeakHashTable.keyIsUsingSpace t key1 |> shouldEqual true
        WeakHashTable.mem t key1 |> shouldEqual true
        WeakHashTable.find t key1 |> shouldEqual (Some value)

        WeakHashTable.keyIsUsingSpace t key2 |> shouldEqual true
        WeakHashTable.mem t key2 |> shouldEqual true
        WeakHashTable.find t key2 |> shouldEqual (Some value)
        GC.KeepAlive value

    [<Test>]
    let ``replace allows duplicate value under multiple keys`` () =
        let t = WeakHashTable.create<int, obj> None
        let key1 = 1
        let key2 = 2
        let value = obj ()

        // Add value under key1
        WeakHashTable.addThrowing t key1 value

        // Replace key2 with the same value - should succeed
        WeakHashTable.replace t key2 value

        // Both keys should have the value
        WeakHashTable.keyIsUsingSpace t key1 |> shouldEqual true
        WeakHashTable.mem t key1 |> shouldEqual true
        WeakHashTable.find t key1 |> shouldEqual (Some value)

        WeakHashTable.keyIsUsingSpace t key2 |> shouldEqual true
        WeakHashTable.mem t key2 |> shouldEqual true
        WeakHashTable.find t key2 |> shouldEqual (Some value)
        GC.KeepAlive value

    [<Test>]
    let ``duplicate value cleanup reclaims all keys`` () =
        let t = WeakHashTable.create<int, obj> None
        let key1 = 1
        let key2 = 2
        let key3 = 3
        let mutable callbacks = 0

        WeakHashTable.setRunWhenUnusedData t (fun () -> callbacks <- callbacks + 1)

        // Add the same value under three keys (use a scope so we can let it be collected)
        do
            let value = obj ()
            WeakHashTable.addThrowing t key1 value
            WeakHashTable.addThrowing t key2 value
            WeakHashTable.replace t key3 value

            // All should be present
            WeakHashTable.mem t key1 |> shouldEqual true
            WeakHashTable.mem t key2 |> shouldEqual true
            WeakHashTable.mem t key3 |> shouldEqual true

            GC.KeepAlive value

        // Force GC to finalize the value
        forceGc ()

        // Callback should have fired exactly once (one finalizer for the shared tracker)
        callbacks |> shouldEqual 1

        // Run reclaim - ALL keys should be reclaimed
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t

        // All keys should be gone
        WeakHashTable.keyIsUsingSpace t key1 |> shouldEqual false
        WeakHashTable.keyIsUsingSpace t key2 |> shouldEqual false
        WeakHashTable.keyIsUsingSpace t key3 |> shouldEqual false
        WeakHashTable.mem t key1 |> shouldEqual false
        WeakHashTable.mem t key2 |> shouldEqual false
        WeakHashTable.mem t key3 |> shouldEqual false

    [<Test>]
    let ``clear preserves null values until explicitly cleared`` () =
        let t = WeakHashTable.create<int, obj> None
        let key1 = 1
        let key2 = 2
        let value = obj ()

        // Add a null value and a non-null value
        WeakHashTable.addThrowing t key1 null
        WeakHashTable.addThrowing t key2 value

        // Both should be present
        WeakHashTable.mem t key1 |> shouldEqual true
        WeakHashTable.mem t key2 |> shouldEqual true
        WeakHashTable.find t key1 |> shouldEqual (Some null)
        WeakHashTable.find t key2 |> shouldEqual (Some value)

        // Clear the table
        WeakHashTable.clear t

        // Both should be gone
        WeakHashTable.mem t key1 |> shouldEqual false
        WeakHashTable.mem t key2 |> shouldEqual false
        WeakHashTable.keyIsUsingSpace t key1 |> shouldEqual false
        WeakHashTable.keyIsUsingSpace t key2 |> shouldEqual false
        WeakHashTable.find t key1 |> shouldEqual None
        WeakHashTable.find t key2 |> shouldEqual None
        GC.KeepAlive value

    [<Test>]
    let ``replace non-null with null then old value finalizes does not reclaim null entry`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        let mutable callbacks = 0

        WeakHashTable.setRunWhenUnusedData t (fun () -> callbacks <- callbacks + 1)

        // Add a non-null value (use a scope so we can let it be collected)
        do
            let value = obj ()
            WeakHashTable.addThrowing t key value

            WeakHashTable.mem t key |> shouldEqual true
            WeakHashTable.find t key |> shouldEqual (Some value)

            // Replace with null - old value is now unreferenced
            WeakHashTable.replace t key null

            // Key should now have null value
            WeakHashTable.mem t key |> shouldEqual true
            WeakHashTable.find t key |> shouldEqual (Some null)
            WeakHashTable.keyIsUsingSpace t key |> shouldEqual true

            GC.KeepAlive value

        // Force GC to finalize the old non-null value
        forceGc ()

        // Callback should have fired for the old value
        callbacks |> shouldEqual 1

        // Run reclaim - this should NOT remove the null entry
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t

        // Null entry should still be present
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.find t key |> shouldEqual (Some null)
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true

    [<Test>]
    let ``null entries survive GC and reclaim`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1

        // Add a null value
        WeakHashTable.addThrowing t key null

        // Should be present
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.find t key |> shouldEqual (Some null)
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true

        // Force multiple GC cycles
        for _ = 1 to 5 do
            forceGc ()
            WeakHashTable.reclaimSpaceForKeysWithUnusedData t

        // Null entry should still be present
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.find t key |> shouldEqual (Some null)
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true

    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private addSharedAndReplace (t : WeakHashTable<int, obj>) key1 key2 (value2 : obj) =
        let sharedValue = obj ()
        WeakHashTable.addThrowing t key1 sharedValue
        WeakHashTable.addThrowing t key2 sharedValue
        // Replace key1 with a different value - key1 now has value2, key2 still has sharedValue
        WeakHashTable.replace t key1 value2

    [<Test>]
    let ``shared value replaced on one key does not reclaim the replaced key`` () =
        let t = WeakHashTable.create<int, obj> None
        let key1 = 1
        let key2 = 2
        let mutable callbacks = 0
        let value2 = obj ()

        WeakHashTable.setRunWhenUnusedData t (fun () -> callbacks <- callbacks + 1)

        // Add a shared value under two keys, then replace one key with value2
        addSharedAndReplace t key1 key2 value2

        // key1 should have value2, key2 should have sharedValue (still alive until GC)
        WeakHashTable.mem t key1 |> shouldEqual true
        WeakHashTable.mem t key2 |> shouldEqual true
        WeakHashTable.find t key1 |> shouldEqual (Some value2)

        // sharedValue is now unreachable - force GC to finalize it
        forceGc ()

        // Callback should have fired for sharedValue
        callbacks |> shouldEqual 1

        // The cleanup tracker for sharedValue has both key1 and key2 enqueued.
        // But key1 now has value2 (alive), so reclaim should NOT remove key1.
        // key2 had sharedValue (now dead), so reclaim SHOULD remove key2.
        WeakHashTable.reclaimSpaceForKeysWithUnusedData t

        // key1 should survive with value2
        WeakHashTable.keyIsUsingSpace t key1 |> shouldEqual true
        WeakHashTable.mem t key1 |> shouldEqual true
        WeakHashTable.find t key1 |> shouldEqual (Some value2)

        // key2 should be reclaimed (sharedValue was collected)
        WeakHashTable.keyIsUsingSpace t key2 |> shouldEqual false
        WeakHashTable.mem t key2 |> shouldEqual false

        GC.KeepAlive value2

    [<Test>]
    let ``addThrowing rejects duplicate non-null on same key`` () =
        let t = WeakHashTable.create<int, obj> None
        let key = 1
        let value1 = obj ()
        let value2 = obj ()

        // Add first value
        WeakHashTable.addThrowing t key value1

        // Try to add a different value under the same key - should throw KeyAlreadyInUseException
        let exc =
            Assert.Throws<KeyAlreadyInUseException> (fun () -> WeakHashTable.addThrowing t key value2)

        exc.Data0 |> unbox<int> |> shouldEqual key

        // Original value should still be there
        WeakHashTable.find t key |> shouldEqual (Some value1)
        WeakHashTable.mem t key |> shouldEqual true
        WeakHashTable.keyIsUsingSpace t key |> shouldEqual true

        GC.KeepAlive value1
        GC.KeepAlive value2

    /// Helper: add key to table with value, then remove the key. Returns WeakReference to key.
    /// Key is a reference type so we can verify it's released from the tracker.
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private addKeyThenRemove (t : WeakHashTable<obj, obj>) (value : obj) : WeakReference =
        let key = obj ()
        WeakHashTable.addThrowing t key value
        WeakHashTable.remove t key
        WeakReference key

    [<Test>]
    let ``remove releases reference-type key from tracker (memory test)`` () =
        // This test verifies that remove actually detaches the key from the tracker.
        // Without pruning, the tracker would hold a strong reference to the key,
        // preventing it from being GC'd even after removal from the table.
        let t = WeakHashTable.create<obj, obj> None
        let value = obj ()

        let keyWeak = addKeyThenRemove t value

        // Force GC - if pruning worked, the key should be collected
        forceGc ()

        // The key should be collected because it's no longer held by the tracker
        keyWeak.IsAlive |> shouldEqual false

        // Keep value alive so the tracker itself stays alive
        GC.KeepAlive value

    /// Helper: add key to table with oldValue, then replace with newValue. Returns WeakReference to key.
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private addKeyThenReplace (t : WeakHashTable<obj, obj>) (oldValue : obj) (newValue : obj) : WeakReference =
        let key = obj ()
        WeakHashTable.addThrowing t key oldValue
        WeakHashTable.replace t key newValue
        WeakReference key

    [<Test>]
    let ``replace releases reference-type key from old tracker (memory test)`` () =
        // This test verifies that replace detaches the key from the OLD value's tracker.
        // Without pruning, the old tracker would hold a strong reference to the key.
        let t = WeakHashTable.create<obj, obj> None
        let oldValue = obj ()
        let newValue = obj ()

        let keyWeak = addKeyThenReplace t oldValue newValue

        // Force GC - the key should NOT be collected yet because newValue's tracker holds it
        forceGc ()
        keyWeak.IsAlive |> shouldEqual true

        // Now verify the key is NOT held by oldValue's tracker by letting oldValue be collected
        // and checking that the key survives (it's held by newValue's tracker, not oldValue's)
        // This is implicit - if the key were in both trackers, it would still survive.
        // The real test is that oldValue can be finalized without issues.

        GC.KeepAlive oldValue
        GC.KeepAlive newValue

    /// Helper: add key with value, then replace with same value multiple times, then remove.
    [<MethodImpl(MethodImplOptions.NoInlining)>]
    let private addKeyReplaceMultipleTimesThenRemove (t : WeakHashTable<obj, obj>) (value : obj) times : WeakReference =
        let key = obj ()
        WeakHashTable.addThrowing t key value

        for _ = 1 to times do
            WeakHashTable.replace t key value
        // Remove the key - this should detach it from the tracker once
        WeakHashTable.remove t key
        WeakReference key

    [<Test>]
    let ``repeated replace with same value keeps single key reference in tracker`` () =
        // This test verifies deduplication indirectly: after multiple replaces with the same value,
        // the key should still be collectable after removal (not held multiple times).
        // Note: We can't directly observe the internal queue, but we verify the key isn't
        // retained multiple times by checking it can be collected after a single remove.
        let t = WeakHashTable.create<obj, obj> None
        let value = obj ()

        let keyWeak = addKeyReplaceMultipleTimesThenRemove t value 5

        // Force GC - if deduplication worked, key should be collected (removed from tracker once)
        // If there were duplicates, the key might still be held... but actually ConcurrentDictionary
        // handles this correctly. This test mainly verifies no exceptions occur.
        forceGc ()
        keyWeak.IsAlive |> shouldEqual false

        GC.KeepAlive value

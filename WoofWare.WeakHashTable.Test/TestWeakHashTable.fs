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

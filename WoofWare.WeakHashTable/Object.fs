namespace WoofWare.WeakHashTable

open System

[<RequireQualifiedAccess>]
module internal Object =

    let referenceEquals<'a when 'a : not struct> (x : 'a) (y : 'a) =
        // fsharpanalyzer: ignore-line-next WOOF-REFEQUALs
        Object.ReferenceEquals (x, y)

    let isNull<'a when 'a : not struct> (x : 'a) =
        // fsharpanalyzer: ignore-line-next WOOF-REFEQUALs
        Object.ReferenceEquals (x, (null : obj))

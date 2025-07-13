# WoofWare.WeakHashTable

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.WeakHashTable.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.WeakHashTable)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.WeakHashTable/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.WeakHashTable/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.WeakHashTable)](./LICENCE.md)

This is the weak hash table from [janestreet/core_kernel](https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184/weak_hashtbl).

To quote the original documentation:

> A single-module library with a hashtable that keeps a weak pointer to each key's data and uses a finalizer to detect when the data is no longer referenced (by any non-weak pointers).

# Licence

This is a derivative work of [core_kernel](https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184), used under the MIT licence.
A copy of that licence is at [LICENCE_janestreet.md](LICENCE_janestreet.md).
All glory to Jane Street.

WoofWare.WeakHashTable is licenced to you under the MIT licence.
A copy of that licence is at [LICENCE.md](LICENCE.md).

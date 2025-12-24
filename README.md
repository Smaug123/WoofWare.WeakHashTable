# WoofWare.WeakHashTable

[![NuGet version](https://img.shields.io/nuget/v/WoofWare.WeakHashTable.svg?style=flat-square)](https://www.nuget.org/packages/WoofWare.WeakHashTable)
[![GitHub Actions status](https://github.com/Smaug123/WoofWare.WeakHashTable/actions/workflows/dotnet.yaml/badge.svg)](https://github.com/Smaug123/WoofWare.WeakHashTable/actions?query=branch%3Amain)
[![License file](https://img.shields.io/github/license/Smaug123/WoofWare.WeakHashTable)](./LICENCE.md)

<picture>
  <source media="(prefers-color-scheme: dark)" srcset="logos/dark.svg">
  <source media="(prefers-color-scheme: light)" srcset="logos/light.svg">
  <img alt="Project logo: minimalistic face of a cartoon Shiba Inu; its right-hand edge is composed of neatly justified horizontal lines." src="logos/light.svg" width="300">
</picture>

This is the weak hash table from [janestreet/core_kernel](https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184/weak_hashtbl).

To quote the original documentation:

> A single-module library with a hashtable that keeps a weak pointer to each key's data and uses a finalizer to detect when the data is no longer referenced (by any non-weak pointers).

# Status

I haven't thought very hard about this port; all the original tests have been ported and pass, but this library has seen no prod use, and for all I know is completely useless on .NET.
Caveat emptor.

# Licence

This is a derivative work of [core_kernel](https://github.com/janestreet/core_kernel/tree/774a6821b14cbcdcde02cbbca1984ea32bf06184), used under the MIT licence.
A copy of that licence is at [LICENCE_janestreet.md](LICENCE_janestreet.md).
All glory to Jane Street.

WoofWare.WeakHashTable is licenced to you under the MIT licence.
A copy of that licence is at [LICENCE.md](LICENCE.md).

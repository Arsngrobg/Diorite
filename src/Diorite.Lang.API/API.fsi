// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    API.fsi
// Summary: The public-facing API for the Diorite language - to be used over messing with language internals
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API

open Diorite.Lang.Core // the core library

[<AutoOpen>]
module API =
     // val tokensOf:   string -> TokenStream
     // val treeOf:     string -> AST
     // val eval:       string -> ValueType list Result
     // val compile:    string -> IR Result
     // val getMemory:  unit   -> Memory
     val getVersion: unit   -> Properties.Version

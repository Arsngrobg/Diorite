// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    API.fs
// Summary: The public-facing API for the Diorite language - to provide a safe abstraction layer over internals
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API

open Diorite.Lang.Core // the core library

/// <summary>
///     <p>The public-facing API for <b>Diorite</b>.</p>
/// </summary>
[<AutoOpen>]
module API =
     // val tokensOf:    string      -> TokenStream
     // val tokensError: TokenStream -> DioriteError list
     // val treeOf:      string      -> AST            Result
     // val initRuntime: Runtime.Configuration -> Evaluator
     // val compile:     string      -> IR             Result

     /// <summary>
     ///    <p>Gets the version of <b>Diorite</b> that this API exposes.</p>
     /// </summary>
     /// <returns> the version of <b>Diorite</b> this API exposes </returns>
     let getVersion (): Properties.Version =
         Properties.languageVersion

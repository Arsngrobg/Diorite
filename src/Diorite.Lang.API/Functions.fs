// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Functions.fs
// Summary: The functions that the end-user should preferably use when using Diorite
// Author:  Arsngrobg
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.API

/// <summary>
///     <p>The set of functions that is intended for the end-user to use when embedding <b>Diorite</b>.</p>
/// </summary>
[<AutoOpen>]
module Functions =
     open Diorite.Lang.Core

     /// <summary>
     ///    <p>Gets the current version of <b>Diorite</b> this API exposes.</p>
     /// </summary>
     /// <returns> the version of <b>Diorite</b> this API exposes </returns>
     let getVersion (): Properties.Version =
         Properties.languageVersion

     // val getVersion:  unit                  -> Properties.Version
     // val tokenStr:    Token                 -> string
     // val treeStr:     AST                   -> string
     // val tokensOf:    string                -> TokenStream
     // val tokenErrors: TokenStream           -> DioriteError list
     // val treeOf:      string                -> Result<AST, DioriteError>
     // val initRuntime: Runtime.Configuration -> Evaluator
     // val compile:     string                -> Result<IR, DioriteError>
     do ()

// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Error.fs
// Summary: Implementation details for Errors.fsi
// Author:  Arsngrobg
// Version: v1.5
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

// marked as AutoOpen as used across the entire project
[<AutoOpen>]
module Errors =
    // Implementation
    type DioriteError =
        | MathError   of string // caused by division by zero for example
        | SyntaxError of string // caused by illegal tokens or illegal token pattern
        | SystemError of string // illegal state caused by external interop code

    // Implementation
    type Result<'a> = Result<'a, DioriteError>

    // Implementation
    let inline MathError<'a> (msg: string): Result<'a> =
        Error (MathError msg)

    // Implementation
    let inline SyntaxError<'a> (msg: string): Result<'a> =
        Error (SyntaxError msg)

    // Implementation
    let inline SystemError<'a> (msg: string): Result<'a> =
        Error (SystemError msg)

    // Implementation
    let inline resultAsBool (result: unit Result): bool =
        match result with
         | Ok    _ -> true
         | Error _ -> false

    // Implementation
    let inline getOrElse<'a> (result: 'a Result) (alternative: 'a): 'a =
        match result with
         | Ok    value -> value
         | Error _     -> alternative

    // Implementation
    let inline generalized<'a> (result: 'a Result): unit Result =
        match result with
         | Error e -> Error e
         | Ok    _ -> Ok    ()

    // Implementation
    [<System.Obsolete("Do not use this - use getOrElse instead!")>]
    let forceUnwrap<'a> (result: 'a Result): 'a =
        match result with
         | Error err   -> raise <| System.Exception $"Result failed{err}"
         | Ok    value -> value
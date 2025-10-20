// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Transformers.fs
// Summary: The transformers for converting from one type to another
// Author:  Arsngrobg, Borngle
// Version: v1.3
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

// separate module for helper functions since we have a lot of those
[<RequireQualifiedAccess>]
module private Transformers =
    let stringToChars (str: string): char list =
        [ for c in str do c ]

    let parseDigit (c: char): int =
        int c - int '0'

    let charsToString (chars: char list): string =
        System.String.Concat chars

    let parseNumber (str: char list): float =
        str |> charsToString |> System.Double.Parse

// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    Predicates.fs
// Summary: The boolean tests for determining whether we should keep consuming a sequence of characters
// Author:  Arsngrobg, Borngle
// Version: v1.2
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang

// predicates for the consume function
[<RequireQualifiedAccess>]
module private Predicates =
    let isLetter (c: char): bool =
        System.Char.IsLetter c

    let isDigit (c: char): bool =
        System.Char.IsDigit c

    let isBlank (c: char): bool =
        c <> '\n' && System.Char.IsWhiteSpace c

    let untilNewline (c: char): bool =
        c <> '\n'

    let any (c: char): bool =
        not(isBlank c)
// ------------------------------------------------------------------------------------------------------------------
//    _____  __              __ __
//   |     \|__|.-----.----.|__|  |_.-----.
//   |  --  |  ||  _  |   _||  |   _|  -__|
//   |_____/|__||_____|__|  |__|____|_____|
//
// ------------------------------------------------------------------------------------------------------------------
// File:    AssemblyInfo.fs
// Summary: Assembly info such as metadata and only allowing the API layer to interact with core functionality
// Author:  Arsngrobg
// Version: v1.0
// ------------------------------------------------------------------------------------------------------------------
// Developed and Created by James Armstrong (Arsngrobg) and Aidan Barden (Borngle) (2025)
// ------------------------------------------------------------------------------------------------------------------

namespace Diorite.Lang.Core

open System.Reflection
open System.Runtime.CompilerServices

[<assembly: AssemblyTitle       "Diorite.Lang.Core"                                      >]
[<assembly: AssemblyDescription "Core functionality for the Diorite mathematics language">]
[<assembly: AssemblyProduct     "Diorite"                                                >]

[<assembly: InternalsVisibleTo  "Diorite.Lang.API"                                       >]

do ()

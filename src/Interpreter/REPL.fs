module Diorite.Interpreter.REPL

open Diorite.Core.Meta.version

[<EntryPoint>]
let main(_: array<string>): int32 =
    printf $"Diorite REPL (v{LANGUAGE_VERSION.asString()})"
    0

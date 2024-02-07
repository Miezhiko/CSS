module CSSCore

open System
open System.ComponentModel

let mutable pasteNewLine  = false
let mutable obfuscation   = false

let mutable tab = "  "

let inline (++) a b = sprintf "%s %s" a b
let inline s str    = str.ToString()

module CSS

open System
open System.IO
open System.Text.RegularExpressions

let prediction =
"""/*--------------------------------------------*/
/* This file is generated */
/*--------------------------------------------*/
"""

let inline (~+.) xs = 
  match xs with
  | []  -> ""
  | [_] -> sprintf " { %s }" xs.Head
  | _   -> 
    let pars = 
      [for s in xs -> 
        sprintf "%s%s" 
        <| tab
        <| s]
    sprintf " {%s%s%s%s}" 
    <| System.Environment.NewLine
    <| String.Join(System.Environment.NewLine, pars)
    <| System.Environment.NewLine
    <| tab

let inline (@) el st =
  sprintf "%s { %s }" el st
let inline (-|) el st =
  sprintf "%s%s"
    <| el
    <| +. st

let inline (@@) el st =
  match el with
    | [] -> ""
    | [ _ ] -> el.Head @ st
    | _ -> String.Join(", ", el) @ st
let inline (=|) el st =
  match el with
    | [] -> ""
    | [ _ ] -> el.Head -| st
    | _ -> String.Join(", ", el) -| st

let inline (--) el st =
  sprintf "%s: %s;"
    <| el 
    <| st.ToString()
let inline (-/) el st =
  sprintf "%s: %s;"
    <| el 
    <| String.Join(" ", (st : string list))
let inline (-+) el st =
  sprintf "%s: %s;"
    <| el 
    <| String.Join(", ", (st : string list))

let inline (%) el p =
  sprintf "%s:%s" el p
let inline (~%) a = sprintf ":%s" a

let inline (-.) el p =
  sprintf "%s.%s" el p
let inline (~-.) a = sprintf ".%s" a

let inline (~+) a = sprintf " %s" a

let inline ($) a b = sprintf "%s, %s" a b

let inline (.>) a b = sprintf "%s > %s" a b
let inline (.<) a b = sprintf "%s < %s" a b

let inline (><) a b = sprintf "%s*%s" a b
let inline (<<) el els =
  let tree str = 
    let lines =
      [for line in Regex.Split(str, "\r\n") ->
        match line with 
        | line when line.StartsWith(tab) -> line
        | _ -> el + line]
    String.Join(System.Environment.NewLine, lines)
  let fall = [for e : string in els ->
          match e with
          | e when e.Contains("*") -> 
            let ells = e.Split('*')
            String.Join(", ",
              [for ell in ells ->
                tree ell])
          | _ -> tree e]
  String.Join(System.Environment.NewLine, fall)

let SS xs =
  let plaincss =
      xs
      |> Seq.map(fun str -> 
        match pasteNewLine with
        | false ->  s str
        | true ->   sprintf "%s%s"
              <| System.Environment.NewLine
              <| s str)
  match obfuscation with
  | true -> obfuscate plaincss
  | false -> String.Join(System.Environment.NewLine, plaincss)

exception InnerError of string
let CSS file xs = 
  if File.Exists file then
    File.WriteAllText(file, (prediction + ( SS xs )))
  else 
    printfn "file %s doesn't exists, create it? (Y/N)" file
    let rec checkA() =
      match Console.ReadLine() with
      | "Y" -> File.WriteAllText(file, (prediction + ( SS xs )))
      | "N" -> ()
      | _ -> printfn "what?"; checkA()
    checkA()

let CSSS xs = CSS "site.css" xs

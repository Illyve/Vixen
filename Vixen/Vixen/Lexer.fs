module Lexer

open System

type Token =
    | IntLit of int
    | StringLit of string
    | Plus | Minus | Star | Slash

type Position = { Line : int; Column : int }

type Context = { Input : char list; Position : Position }

let NextChar context =
    match context.Input with
    | [] -> { Input = []; Position = { Line = context.Position.Line; Column = context.Position.Column } }
    | '\n' :: tail -> { Input = tail; Position = { Line = context.Position.Line + 1; Column = 0 } }
    | _ :: tail -> { Input = tail; Position = { Line = context.Position.Line; Column = context.Position.Column + 1 } }

let (|IntegerLiteral|_|) context =
    let rec Loop context acc =
        match context.Input with
        | i :: tail when Char.IsDigit i -> Loop (NextChar context) (i :: acc)
        | _ -> (acc |> List.rev, context)
    match Loop context [] with
    | ([], _) -> None
    | (digits, tail) -> 
        let i = digits |> List.toArray |> String |> Int32.Parse
        Some (IntLit i, tail, context.Position)

let (|StringLiteral|_|) context =
    let rec Loop context acc =
        match context.Input with
        | i :: tail when Char.IsLetterOrDigit i -> Loop (NextChar context) (i :: acc)
        | _ -> (acc |> List.rev, context)
    match Loop context [] with
    | ([], _) -> None
    | (digits, tail) -> 
        let str = digits |> Array.ofList |> String
        Some (StringLit str, tail, context.Position)

let (|Symbol|_|) context =
    match context.Input with
    | '+' :: tail -> Some (Plus, NextChar context, context.Position)
    | '-' :: tail -> Some (Minus, NextChar context, context.Position)
    | '*' :: tail -> Some (Star, NextChar context, context.Position)
    | '/' :: tail -> Some (Slash, NextChar context, context.Position)
    | _ -> None

let Lex input =
    let rec Loop input acc =
        match input with
        | IntegerLiteral (i, con, pos) -> Loop con ((i, pos)  :: acc)
        | StringLiteral (s, con, pos) -> Loop con ((s, pos) :: acc)
        | Symbol (s, con, pos) -> Loop con ((s, pos) :: acc)
        | { Input = head :: tail; Position = _ } as con -> Loop (NextChar con) acc
        | { Input = []; Position = _ } -> acc |> List.rev
    Loop ({ Input = input |> Seq.toList; Position = { Line = 1; Column = 1 } }) []


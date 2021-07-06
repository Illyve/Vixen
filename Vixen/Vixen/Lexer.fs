module Lexer

open System

type Token =
    | IntLit of int
    | StringLit of string
    | Keyword of string
    | Identifier of string
    | Assign
    | Plus | Minus | Star | Slash
    | Equals | NotEquals | LessThan | GreaterThan | LessEquals | GreaterEquals
    | LeftBrace | RightBrace
    | LeftParen | RightParen
    | LeftBracket | RightBracket
    | Period

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
    match context.Input with
    | '\"' :: tail ->
        let rec Loop context acc =
            match context.Input with
            | i :: tail when i <> '\"' -> Loop (NextChar context) (i :: acc)
            | _ -> (acc |> List.rev, NextChar context)
        let con = NextChar context
        match Loop con [] with
        | ([], _) -> None
        | (digits, tail) -> 
            let str = digits |> Array.ofList |> String
            Some (StringLit str, tail, context.Position)
    | _ -> None

let (|Symbol|_|) context =
    match context.Input with
    | '+' :: tail -> Some (Plus, NextChar context, context.Position)
    | '-' :: tail -> Some (Minus, NextChar context, context.Position)
    | '*' :: tail -> Some (Star, NextChar context, context.Position)
    | '/' :: tail -> Some (Slash, NextChar context, context.Position)
    | '=' :: '=' :: tail -> Some (Equals, NextChar context, context.Position)
    | '=' :: tail -> Some (Assign, NextChar context, context.Position)
    | '!' :: '=' :: tail -> Some (NotEquals, NextChar context, context.Position)
    | '<' :: '=' :: tail -> Some (LessEquals, NextChar context, context.Position)
    | '>' :: '=' :: tail -> Some (GreaterEquals, NextChar context, context.Position)
    | '<' :: tail -> Some (LessThan, NextChar context, context.Position)
    | '>' :: tail -> Some (GreaterThan, NextChar context, context.Position)
    | '{' :: tail -> Some (LeftBrace, NextChar context, context.Position)
    | '}' :: tail -> Some (RightBrace, NextChar context, context.Position)
    | '(' :: tail -> Some (LeftParen, NextChar context, context.Position)
    | ')' :: tail -> Some (RightParen, NextChar context, context.Position)
    | '[' :: tail -> Some (LeftBracket, NextChar context, context.Position)
    | ']' :: tail -> Some (RightBracket, NextChar context, context.Position)
    | '.' :: tail -> Some (Period, NextChar context, context.Position)
    | _ -> None

let GetString context =
    let rec Loop context acc =
        match context.Input with
        | c :: tail when Char.IsLetterOrDigit c -> Loop (NextChar context) (c :: acc)
        | _ -> (acc |> List.rev, context)
    match Loop context [] with
    | ([], _) -> None
    | (digits, tail) -> 
        let str = digits |> Array.ofList |> String
        Some (str, tail, context.Position)

let (|Keyword|_|) context =
    match GetString context with
    | Some ("decl", tail, pos) -> Some (Token.Keyword "decl", tail, pos)
    | Some ("if", tail, pos) -> Some (Token.Keyword "if", tail, pos)
    | Some ("while", tail, pos) -> Some (Token.Keyword "while", tail, pos)
    | Some ("for", tail, pos) -> Some (Token.Keyword "for", tail, pos)
    | Some ("return", tail, pos) -> Some (Token.Keyword "return", tail, pos)
    | _ -> None

let (|Identifier|_|) context =
    match GetString context with
    | Some (ident, tail, pos) -> Some (Token.Identifier ident, tail, pos)
    | _ -> None

let Lex input =
    let rec Loop input acc =
        match input with
        | IntegerLiteral (i, con, pos) -> Loop con ((i, pos)  :: acc)
        | StringLiteral (s, con, pos) -> Loop con ((s, pos) :: acc)
        | Symbol (s, con, pos) -> Loop con ((s, pos) :: acc)
        | Keyword (k, con, pos) -> Loop con ((k, pos) :: acc)
        | Identifier (i, con, pos) -> Loop con ((i, pos) :: acc)
        | { Input = head :: tail; Position = _ } as con -> Loop (NextChar con) acc
        | { Input = []; Position = _ } -> acc |> List.rev
    Loop ({ Input = input |> Seq.toList; Position = { Line = 1; Column = 1 } }) []


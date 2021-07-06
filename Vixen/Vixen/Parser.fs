module Parser

open Lexer

type ParseResult<'T> =
    | Success of 'T * (Token * Position) list
    | Failure of string

let ( >>= ) first second =
   let Parse input =
        match first input with
        | Success (value, remaning) -> second value remaning
        | Failure err -> Failure err
   Parse

let Return value input =
    Success (value, input)

type ParseTree =
    | IntLit of int
    | StringLit of string
    | BinaryExpression of ParseTree * Token * ParseTree

let ParsePrimary input = 
    match input with
    | (Token.IntLit i, _) :: tail -> Success (IntLit i, tail)
    | (Token.StringLit s, _) :: tail -> Success (StringLit s, tail)
    | (token, { Line = line; Column = col }) :: tail -> Failure $"Expected primary on line: {line} column: {col}, got {token}."
    | [] -> Failure $"Expected primary, got EOF."

let ParseTokens tokens input =
    match input with
    | (token, _) :: tail when tokens |> List.contains token -> Success (token, tail)
    | (token, { Line = line; Column = col }) :: _ -> Failure $"Expected tokens {tokens} on line: {line} column: {col}, got {token}."
    | [] -> Failure $"Expected tokens {tokens}, got EOF."

let ZeroOrMore parser =
    let rec Parse acc input =
        match parser input with
        | Success (value, remaining) -> 
            Parse (value :: acc) remaining
        | Failure _ -> Success (acc, input)
    Parse []

#nowarn "40"

let rec ParseMulExp =
    ParsePrimary >>= fun left ->
    (ZeroOrMore (ParseTokens [ Token.Star; Token.Slash ] >>= fun op ->
        ParseMulExp >>= fun right ->
        Return (op, right))) >>= fun nexts ->
    let exp = nexts |> List.fold (fun left (op, right) -> BinaryExpression(left, op, right)) left
    Return exp

let rec ParseAddExp =
    ParseMulExp >>= fun left ->
    (ZeroOrMore (ParseTokens [ Token.Plus; Token.Minus ] >>= fun op ->
        ParseAddExp >>= fun right ->
        Return (op, right))) >>= fun nexts ->
    let exp = nexts |> List.fold (fun left (op, right) -> BinaryExpression(left, op, right)) left
    Return exp

let ParseExpression =
    ParseAddExp

#warn "40"

let Parse input =
    match ParseExpression input with
    | Success (value, _) -> value
    | Failure err -> failwith err
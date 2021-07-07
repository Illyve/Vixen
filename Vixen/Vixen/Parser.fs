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

let Lift value input =
    Success (value, input)

let ZeroOrMore parser =
    let rec Parse acc input =
        match parser input with
        | Success (value, remaining) -> 
            Parse (value :: acc) remaining
        | Failure _ -> Success (acc, input)
    Parse []

type Statement =
    | GlobalDeclaration of string * Expression
    | Declaration of string * Expression
    | FunctionDeclaration of string * Expression list * Statement
    | StructureDefinition of string * Statement list
    | FunctionCall of string * Expression list
    | Assignment of string * Expression
    | If of Expression * Statement
    | IfElse of Expression * Statement * Statement
    | While of Expression * Statement
    | For of Statement * Expression * Statement * Statement
    | Return of Expression
    | Compound of Statement list
and Expression =
    | IntLit of int
    | StringLit of string
    | Identifier of string
    | Array of string * Expression
    | Object of string
    | FunctionCall of string * Expression list
    | Parameter of string * string
    | BinaryExpression of Expression * Token * Expression

let ParseIdentifierAsString input =
    match input with
    | (Token.Identifier ident, _) :: tail -> Success (ident, tail)
    | (token, { Line = line; Column = col }) :: _ -> Failure $"Expected identifier on line: {line} column: {col}, got {token}."
    | [] -> Failure "Expected identifier, got EOF."

let ParseTokens tokens input =
    match input with
    | (token, _) :: tail when tokens |> List.contains token -> Success (token, tail)
    | (token, { Line = line; Column = col }) :: _ -> Failure $"Expected tokens {tokens} on line: {line} column: {col}, got {token}."
    | [] -> Failure $"Expected tokens {tokens}, got EOF."

#nowarn "40"

// Also parses function calls as an expression
let rec ParsePrimary input = 
    match input with
    | (Token.IntLit i, _) :: tail -> Success (IntLit i, tail)
    | (Token.StringLit s, _) :: tail -> Success (StringLit s, tail)
    | (Token.Identifier ident, _) :: tail -> 
        match tail with
        | (Token.LeftParen, _) :: tail ->
            tail 
            |> (ZeroOrMore ParseExpression >>= fun parameters ->
                Lift (FunctionCall (ident, parameters)))
        | _ -> 
            Success (Identifier ident, tail)
    | (Token.Keyword "new", _) :: tail ->
        tail
        |> (ParseIdentifierAsString >>= fun valType remaining ->
            match remaining with
            | (Token.LeftBracket, _) :: tail2 -> 
                tail2 
                |> (ParseExpression >>= fun length ->
                    ParseTokens [ Token.RightBracket ] >>= fun _ ->
                    Lift (Array (valType, length)))
            | _ -> 
                remaining 
                |> Lift (Object valType))
    | (token, { Line = line; Column = col }) :: tail -> Failure $"Expected primary on line: {line} column: {col}, got {token}."
    | [] -> Failure $"Expected primary, got EOF."

and ParseMulExp =
    ParsePrimary >>= fun left ->
    (ZeroOrMore (ParseTokens [ Token.Star; Token.Slash ] >>= fun op ->
        ParseMulExp >>= fun right ->
        Lift (op, right))) >>= fun nexts ->
    let exp = nexts |> List.fold (fun left (op, right) -> BinaryExpression(left, op, right)) left
    Lift exp

and ParseAddExp =
    ParseMulExp >>= fun left ->
    (ZeroOrMore (ParseTokens [ Token.Plus; Token.Minus ] >>= fun op ->
        ParseAddExp >>= fun right ->
        Lift (op, right))) >>= fun nexts ->
    let exp = nexts |> List.fold (fun left (op, right) -> BinaryExpression(left, op, right)) left
    Lift exp

and ParseExpression =
    ParseAddExp

#warn "40"

let ParseKeyword keyword input =
    match input with
    | (Token.Keyword word, _) :: tail when word = keyword -> Success (word, tail)
    | (token, { Line = line; Column = col }) :: _ -> Failure $"Expected keyword {keyword} on line: {line} column: {col}, got {token}."
    | [] -> Failure $"Expected keyword, got EOF."

let ParseDeclaration =
    ParseKeyword "decl" >>= fun _ ->
    ParseIdentifierAsString >>= fun ident ->
    ParseTokens [ Token.Assign ] >>= fun _ ->
    ParseExpression >>= fun exp ->
    Lift (Declaration (ident, exp))

let ParseAssignment =
    ParseIdentifierAsString >>= fun ident ->
    ParseTokens [ Token.Assign ] >>= fun _ ->
    ParseExpression >>= fun exp ->
    Lift (Assignment (ident, exp))

let ParseReturn =
    ParseKeyword "return" >>= fun _ ->
    ParseExpression >>= fun exp ->
    Lift (Return exp)

// Function call as a statement
let ParseFunctionCall  =
    ParseIdentifierAsString >>= fun ident ->
    ParseTokens [ Token.LeftParen ] >>= fun _ ->
    ZeroOrMore ParseExpression >>= fun parameters ->
    ParseTokens [ Token.RightParen ] >>= fun _ ->
    Lift (FunctionCall (ident, parameters))

let rec ParseIf =
    ParseKeyword "if" >>= fun _ ->
    ParseExpression >>= fun cond ->
    ParseCompound >>= fun comp remaining ->
    match remaining with
    | (Token.Keyword "else", _) :: tail -> 
        tail 
        |> (ParseIf >>= fun after ->
            Lift (IfElse (cond, comp, after)))
    | _ -> 
        remaining 
        |> Lift (If (cond, comp))

and ParseWhile =
    ParseKeyword "while" >>= fun _ ->
    ParseExpression >>= fun cond ->
    ParseCompound >>= fun comp ->
    Lift (While (cond, comp))

and ParseFor =
    ParseKeyword "for" >>= fun _ ->
    ParseStatement >>= fun init ->
    ParseExpression >>= fun cond ->
    ParseStatement >>= fun iter ->
    ParseCompound >>= fun comp ->
    Lift (For (init, cond, iter, comp))

and ParseStatement input =
    match input with
    | (Token.Keyword "decl", _) :: tail -> ParseDeclaration input
    | (Token.Keyword "if", _) :: tail -> ParseIf input
    | (Token.Keyword "while", _) :: tail -> ParseWhile input
    | (Token.Keyword "for", _) :: tail -> ParseFor input
    | (Token.Keyword "return", _) :: tail -> ParseReturn input
    | (Token.Identifier ident, _) :: tail -> ParseAssignment input
    | (token, { Line = line; Column = col }) :: tail -> Failure $"Unexpected token {token} on line: {line} column: {col}."
    | [] -> Failure $"Unexpected EOF."

and ParseCompound =
    ParseTokens [ Token.LeftBrace ] >>= fun _ remaining ->
    let rec Cycle acc input =
        match input with
        | (Token.RightBrace, _) :: remaining -> 
            remaining
            |> Lift (Compound acc)
        | _ -> 
            input
            |> (ParseStatement >>= fun stmt ->
                Cycle (stmt :: acc))
    Cycle [] remaining

let ParseParameter = 
    ParseIdentifierAsString >>= fun valType ->
    ParseIdentifierAsString >>= fun ident ->
    Lift (Parameter (valType, ident))

let ParseGlobalDeclaration =
    ParseKeyword "decl" >>= fun _ ->
    ParseIdentifierAsString >>= fun ident ->
    ParseTokens [ Token.Assign ] >>= fun _ remaining ->
    match remaining with
    | (Token.LeftParen, _) :: tail ->
        tail 
        |> (ZeroOrMore ParseParameter >>= fun parameters ->
            ParseTokens [ Token.RightParen ] >>= fun _ ->
            ParseCompound >>= fun comp ->
            Lift (FunctionDeclaration (ident, parameters, comp)))
    | _ -> 
        remaining
        |> (ParseExpression >>= fun exp ->
            Lift (GlobalDeclaration (ident, exp)))

let Parse input =
    let rec Cycle input acc =
        match ParseGlobalDeclaration input with
        | Success (value, []) -> (value :: acc) |> List.rev
        | Success (value, remaining) -> Cycle remaining (value :: acc)
        | Failure err -> failwith err
    Cycle input []
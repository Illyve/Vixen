module Generator

open Lexer
open Parser
open Symbols
open CodeGenerator

let Generate tree context =
    let rec StmtF stmt context =
        match stmt with
        | GlobalDeclaration (ident, exp)  ->
            let con = 
                CgGlobal ident context |>
                match exp with
                | IntLit i -> CgStaticInt i
            { context with Symbols = con.Symbols.Pop }
        | FunctionDeclaration (ident, parameters, comp) ->
            let con =
                CgFunctionStart ident context |>
                StmtF comp |>
                CgFunctionEnd
            { context with Symbols = con.Symbols.Pop }
        | Compound stmts ->
            let con =
                stmts 
                |> List.fold (fun state stmt -> StmtF stmt state) context
            { context with Symbols = con.Symbols.Pop }
        | _ -> failwith "Invalid statement."
    // Evaluates locals
    and ExprF expr context con =
        match expr with
        | IntLit i -> CgLoadInt i context
        | BinaryExpression (left, op, right) ->
            ExprF left context (fun lacc ->
                ExprF right lacc (fun acc -> 
                    match op with
                    | Plus -> CgAdd acc
                    | Minus -> CgSub acc
                    | Star -> CgMul acc
                    | Slash -> CgDiv acc))
    tree 
    |> List.fold (fun cont stmt -> StmtF stmt cont) context
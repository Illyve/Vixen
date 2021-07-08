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
                | StringLit s -> CgStorePooledString context.StringPool.[s]
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
        | Assignment (ident, exp) ->
            ExprF exp context (fun acc ->
            match context.Symbols.[ident] with
            | Global (valType, accessType, storageType, _) -> 
                match valType with
                | "int" -> 
                    CgStoreGlobal ident acc
                | "string" -> 
                    CgLoadGlobal ident acc |>
                    CgStoreReference 
            | Local (valType, accessType, storageType, pos) ->
                CgStoreLocal pos acc)
                
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
        |> con
    let pooled =
        context.StringPool 
        |> Map.fold (fun con s i ->
            CgPoolString s i con) context
    tree
    |> List.fold (fun cont stmt -> StmtF stmt cont) pooled
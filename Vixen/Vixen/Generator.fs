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
                | Array (valType, length) -> CgStaticInt 0
            { con with Symbols = con.Symbols.Pop }
        | FunctionDeclaration (ident, parameters, comp) ->
            let con =
                let con1 = CgFunctionStart ident context
                match ident with
                | "main" -> con1.Stream.WriteLine "\tcall\t__main"
                | _ -> ()
                // Store passed parameters
                let (con2, _) = 
                    parameters
                    |> List.fold (fun (con, i) p -> 
                        let (Expression.Parameter (valType, ident)) = p
                        let (Parameter (_, _, _, pos)) = con.Symbols.[ident]
                        let output = CgStoreParam i pos con
                        (output, i + 1)) ({ con1 with Symbols = con1.Symbols.Pop }, 0)
                con2 |>
                StmtF comp |>
                CgFunctionEnd
            { con with Symbols = con.Symbols.Pop }
        | Declaration (ident, exp) ->
            let con =
                ExprF exp context (fun acc ->
                    match context.Symbols.[ident] with
                    | Global (valType, _, storageType, _) ->
                        CgStoreGlobal ident acc
                    | Local (valType, _, storageType, pos) ->
                        CgStoreLocal pos acc
                    | Parameter (valType, _, storageType, pos) ->
                        CgStoreLocal pos acc)
            { con with Symbols = con.Symbols.Pop }
        | Compound stmts ->
            let con =
                stmts 
                |> List.fold (fun state stmt -> StmtF stmt state) context
            { con with Symbols = con.Symbols.Pop }
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
        | Return exp ->
            ExprF exp context (fun acc ->
                CgLoadReturn acc)
        | Statement.FunctionCall (ident, parameters) ->
            // 1. Evaluates parameters and pushes them onto the stack
            let con1 = 
                parameters 
                |> List.fold (fun con p ->
                    ExprF p con (fun acc ->
                        CgPushStack acc)) context
            // 2. Pops parameters off the stack and loads them backwards
            let rec Cycle i acc =
                match i with
                | 0 -> 
                    acc |>
                    CgPopStack |>
                    CgLoadParam i
                | _ -> 
                    acc |>
                    CgPopStack |>
                    CgLoadParam i |>
                    Cycle (i - 1)
            con1 |> // 1.
            Cycle (parameters.Length - 1) |> // 2.
            CgFunctionCall ident
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
        | Identifier ident ->
            match context.Symbols.[ident] with
            | Global (valType, AccessType.Value, storageType, _) ->
                CgLoadGlobal ident context
            | Global (valType, AccessType.Reference, storageType, _) ->
                CgLoadGlobalReference ident context
            | Local (valType, AccessType.Value, storageType, pos) ->
                CgLoadLocal pos context
            | Local (valType, AccessType.Reference, storageType, pos) ->
                context |>
                CgLoadLocal pos |>
                CgDereference
            | Parameter (valType, AccessType.Value, storageType, pos) ->
                CgLoadLocal pos context
            | Parameter (valType, AccessType.Reference, storageType, pos) ->
                context |>
                CgLoadLocal pos |>
                CgDereference
        | FunctionCall (ident, parameters) ->
            let (newCon, _) =
                parameters |> List.fold (fun (con, i) p ->
                    let newCon = 
                        ExprF p con (fun acc ->
                            CgLoadParam i acc)
                    (newCon, i + 1)) (context, 0)
            newCon |>
            CgFunctionCall ident |>
            CgStoreReturn
        | Array (valType, length) ->
            ExprF length context (fun acc ->
                match context.Symbols.[valType] with
                | Definition (_, _, size, _) -> 
                    acc |>
                    CgLoadInt size |>
                    CgAlloc)
        |> con
    let pooled =
        context.StringPool 
        |> Map.fold (fun con s i ->
            CgPoolString s i con) context
    tree
    |> List.fold (fun cont stmt -> StmtF stmt cont) pooled
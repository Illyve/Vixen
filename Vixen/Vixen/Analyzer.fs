module Analyzer

open Symbols
open Parser

type AnalyzerContext =
    {
        Symbols : Table<Symbol<Expression>>
        StringPool : Set<string>
        ObjectPool : Table<Expression>
        Position : int
    }

let GetType context exp =
    match exp with
    | IntLit i -> "int"
    | StringLit s -> "string"
    | Identifier ident -> 
        match context.Symbols.[ident] with
        | Global (valType, _, _, _) -> valType
        | Local (valType, _, _, _) -> valType
        | Symbol.Parameter (valType, _, _, _) -> valType
        | Member (valType, _, _, _, _) -> valType
        | Definition (_, _, _, _) -> ident
        | Function valType -> valType
    | _ -> failwith "Unexpected type."

let GetSize context valType =
    let (Definition (_, _, size, _)) = context.Symbols.[valType]
    size

let Analyze input =
    let rec StmtF input context  =
        match input with
        | GlobalDeclaration (ident, exp)->
            let con = ExprF exp context
            let sym = Global (Type = GetType con exp, AccessType = AccessType.Value, StorageType = StorageType.Dynamic, Value = exp)
            { con with Symbols = con.Symbols.Add (ident, sym) }
        | FunctionDeclaration (ident, parameters, comp) ->
            let con1 = parameters |> List.fold (fun c p -> ExprF p c) { context with Symbols = context.Symbols.Enter () }
            let valType =
                let (Compound stmts) = comp
                match stmts |> List.tryFind 
                    (fun stmt ->
                        match stmt with
                        | Return e -> true
                        | _ -> false) with
                | Some (Return e) -> GetType con1 e
                | _ -> "void"
            let sym = Function (Type = valType)
            let con2 = StmtF comp con1
            { con2 with Symbols = con2.Symbols.Exit 1; Position = 0 }
        | Declaration (ident, exp) ->
            let valType = GetType context exp
            let pos = 
                context.Position - (GetSize context valType)
            let sym = Local (Type = GetType context exp, AccessType = AccessType.Value, StorageType = StorageType.Dynamic, Position = pos)
            { context with Symbols = context.Symbols.Add(ident, sym) }
        | _ -> { context with Symbols = new Table<Symbol<Expression>> (context.Symbols.List) }
    and ExprF input context =
        match input with
        | StringLit s -> { context with StringPool = context.StringPool.Add s }
        | Parameter (valType, ident) -> 
            let pos = context.Position - (GetSize context valType)
            let sym = Symbols.Parameter (valType, AccessType = AccessType.Value, StorageType = StorageType.Dynamic, Position = pos)
            { context with Symbols = context.Symbols.Add (ident, sym); Position = pos }
        | _ -> context
    let output =
        input 
        |> List.fold (fun c i -> StmtF i c) 
            { 
                Symbols = new Table<Symbol<Expression>> 
                    (
                        [ 
                            [ Map.empty
                            .Add("int", Definition (AccessType = AccessType.Value, StorageType = StorageType.Dynamic, Size = 8, Value = IntLit 0))
                            .Add("string", Definition (AccessType = AccessType.Reference, StorageType = StorageType.Dynamic, Size = 8, Value = StringLit ""))
                            ]
                        ] 
                    )
                StringPool = Set.empty
                ObjectPool = new Table<Expression> ([ [ Map.empty ] ])
                Position = 0 
            }
    { output with Symbols = new Table<Symbol<Expression>> (output.Symbols.List |> List.rev)}
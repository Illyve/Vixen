module CodeGenerator

open System.IO
open Symbols
open Parser

let registers = [ "%r8"; "%r9"; "%r10"; "%r11" ]

type CodeContext = 
    { 
        Symbols : Table<Symbol<Expression>>
        StringPool : List<string>
        ObjectPool : Table<Expression> 
        Stream : StreamWriter
        RegisterStack : string list
    }

let PopRegister context =
    match context.RegisterStack with
    | r :: tail ->
        (r, { context with RegisterStack = tail })
    | _ -> failwith "Failed to pop register."

let PeekRegister context =
    match context.RegisterStack with
    | r :: _ ->
        r
    | _ -> failwith "Failed to peek register."

let AllocRegister context =
    let r = 
        registers |> List.find (fun r -> not (List.contains r context.RegisterStack))
    (r, context)

let CgGlobal x context =
    context.Stream.WriteLine (sprintf "\t.globl\t%s" x)
    context.Stream.WriteLine "\t.data"
    context.Stream.WriteLine (sprintf "%s:" x)
    context

let CgStaticInt x context =
    context.Stream.WriteLine (sprintf "\t.quad %d" x)
    context

let CgStaticString x context =
    context.Stream.WriteLine (sprintf "\t.ascii \"%s\0\"" x)
    context

let CgFunctionStart x context =
    context.Stream.WriteLine "\t.text"
    context.Stream.WriteLine (sprintf "\t.globl\t%s" x)
    context.Stream.WriteLine (sprintf "\t.def\t%s;\t.scl\t2;\t.type\t32;\t.endef" x)
    context.Stream.WriteLine (sprintf "%s:" x)
    context.Stream.WriteLine "\tpushq\t%rbp"
    context.Stream.WriteLine "\tmovq\t%rsp, %rbp"
    context.Stream.WriteLine "\tsubq\t$48, %rsp"
    context.Stream.WriteLine "\tcall\t__main"
    context

let CgFunctionEnd context =
    context.Stream.WriteLine "\taddq\t$48,\t$rsp"
    context.Stream.WriteLine "\tpopq\t%rbp"
    context.Stream.WriteLine "\tret"
    context

let CgLocal context pos =
    let (r, context) = PopRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%s, %d(%%rbp)" r pos)
    context

let CgLoadInt x context = 
    let (r, context) = AllocRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t$%d, %s" x r)
    context

let CgAdd context =
    let (r1, context1) = PopRegister context
    let r2 = PeekRegister context1
    context.Stream.WriteLine (sprintf "\taddq\t%s, %s" r1 r2)
    context1

let CgSub context =
    let (r1, context1) = PopRegister context
    let r2 = PeekRegister context1
    context.Stream.WriteLine (sprintf "\tsubq\t%s, %s" r1 r2)
    context1

let CgMul context =
    let (r1, context1) = PopRegister context
    let r2 = PeekRegister context1
    context.Stream.WriteLine (sprintf "\tmulq\t%s, %s" r1 r2)
    context1

let CgDiv context =
    let (r1, context1) = PopRegister context
    let r2 = PeekRegister context1
    context.Stream.WriteLine (sprintf "\tdivq\t%s, %s" r1 r2)
    context1

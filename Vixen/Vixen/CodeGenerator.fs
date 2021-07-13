module CodeGenerator

open System.IO
open Symbols
open Parser

let registers = [ "%r8"; "%r9"; "%r10"; "%r11" ]
let funcRegisters = [ "%rcx"; "%rdx"; "%r8"; "%r9" ]

type CodeContext = 
    { 
        Symbols : Table<Symbol<Expression>>
        StringPool : Map<string, int>
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
    (r, { context with RegisterStack = r :: context.RegisterStack })

let CgPushStack context =
    let (r, context1) = PopRegister context
    context.Stream.WriteLine (sprintf "\tpushq\t%s" r)
    context1

let CgPopStack context =
    let (r, context1) = AllocRegister context
    context.Stream.WriteLine (sprintf "\tpopq\t%s" r)
    context1

let CgGlobal x context =
    context.Stream.WriteLine (sprintf "\t.globl\t%s" x)
    context.Stream.WriteLine "\t.data"
    context.Stream.WriteLine (sprintf "%s:" x)
    context

let CgStoreGlobal x context =
    let (r, context1) = PopRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%s, %s(%%rip)" r x)
    context

let CgLoadGlobal x context =
    let (r, context1) = AllocRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%s, %s" x r)
    context1

let CgLoadGlobalReference x context =
    let (r, context1) = AllocRegister context
    context.Stream.WriteLine (sprintf "\tleaq\t%s, %s" x r)
    context1

let CgStaticInt x context =
    context.Stream.WriteLine (sprintf "\t.quad\t%d" x)
    context

let CgStaticString x context =
    context.Stream.WriteLine (sprintf "\t.ascii\t\"%s\0\"" x)
    context

let CgPoolString s i context =
    context.Stream.WriteLine (sprintf ".LC%d:" i)
    CgStaticString s context

let CgStorePooledString i context =
    context.Stream.WriteLine (sprintf "\t.quad\t.LC%d" i)
    context

let CgFunctionStart x context =
    context.Stream.WriteLine "\t.text"
    context.Stream.WriteLine (sprintf "\t.globl\t%s" x)
    context.Stream.WriteLine (sprintf "\t.def\t%s;\t.scl\t2;\t.type\t32;\t.endef" x)
    context.Stream.WriteLine (sprintf "%s:" x)
    context.Stream.WriteLine "\tpushq\t%rbp"
    context.Stream.WriteLine "\tmovq\t%rsp, %rbp"
    context.Stream.WriteLine "\tsubq\t$48, %rsp"
    context

let CgFunctionEnd context =
    context.Stream.WriteLine "\taddq\t$48, %rsp"
    context.Stream.WriteLine "\tpopq\t%rbp"
    context.Stream.WriteLine "\tret"
    context

let CgFunctionCall x context =
    context.Stream.WriteLine (sprintf "\tcall\t%s" x)
    context

let CgLoadReturn context =
    let (r, context1) = PopRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%s, %%rax" r)
    context1

let CgStoreReturn context =
    let (r, context1) = AllocRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%%rax, %s" r)
    context1

let CgStoreParam i pos context =
    context.Stream.WriteLine (sprintf "\tmovq\t%s, %d(%%rbp)" funcRegisters.[i] pos)
    context

let CgLoadParam i context =
    let (r, context1) = PopRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%s, %s" r funcRegisters.[i])
    context1

let CgStoreLocal pos context =
    let (r, context1) = PopRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%s, %d(%%rbp)" r pos)
    context1

let CgLoadLocal pos context =
    let (r, context1) = AllocRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%d(%%rbp), %s" pos r)
    context1

let CgDereference context =
    let r = PeekRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t(%s), %s" r r)
    context

let CgStoreReference context =
    let (r1, context1) = PopRegister context
    let (r2, context2) = PopRegister context
    context.Stream.WriteLine (sprintf "\tmovq\t%s, (%s)" r1 r2)
    context2

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
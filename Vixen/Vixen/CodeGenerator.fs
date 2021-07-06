module CodeGenerator

open System.IO

let registers = [ "r8"; "r9"; "r10"; "r11" ]

type GeneratorContext = { Stream : StreamWriter; RegisterStack : string list }

let AllocRegister context =
    registers |> List.find (fun r -> not (List.contains r context.RegisterStack))

let CgLoadInt x context = 
    let r = AllocRegister context
    context.Stream.Write (sprintf "\tmovq\t$%d, %s\n" x r)
    { Stream = context.Stream; RegisterStack = r :: context.RegisterStack }
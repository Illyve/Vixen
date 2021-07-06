open System
open System.IO
open CodeGenerator

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText ("..\\..\\..\\Tests\\test1.txt")
    let tokens = Lexer.Lex input
    let parseTree = Parser.Parse tokens
    let stream = new StreamWriter (File.Open ("..\\..\\..\\Tests\\test1.s", FileMode.Create))
    Generator.Generate parseTree { Stream = stream; RegisterStack = [] }
    stream.Close()
    0
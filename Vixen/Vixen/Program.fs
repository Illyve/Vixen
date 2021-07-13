open System
open System.IO
open CodeGenerator

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText ("..\\..\\..\\Tests\\test1.txt")
    let tokens = Lexer.Lex input
    let parseTree = Parser.Parse tokens
    let acontext = Analyzer.Analyze parseTree
    let stream = new StreamWriter (File.Open ("..\\..\\..\\Tests\\test1.s", FileMode.Create))
    Generator.Generate parseTree 
        { 
            Symbols = acontext.Symbols
            StringPool = acontext.StringPool
                |> Set.toList 
                |> List.mapi (fun i s -> (s, i))
                |> Map.ofList
            ObjectPool = acontext.ObjectPool
            Stream = stream; RegisterStack = [] 
        } |> ignore
    stream.Close ()
    0
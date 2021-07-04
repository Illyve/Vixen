open System
open System.IO

[<EntryPoint>]
let main argv =
    let input = File.ReadAllText ("..\\..\\..\\Tests\\test1.txt")
    let tokens = Lexer.Lex input
    0
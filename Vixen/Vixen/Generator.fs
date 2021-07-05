module Generator

open Lexer
open Parser
open CodeGenerator

let Generate input context =
    match input with
    | IntLit i -> CgLoadInt i context
    ()
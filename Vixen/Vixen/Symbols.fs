module Symbols

type Table<'T> (entries : Map<string, 'T> list list) =
    member this.List = 
        match entries with
        | [] -> [ [ Map.empty ] ]
        | _ -> entries
    member this.Add (key, value) = 
        match this.List.Head with
        | head :: tail when not (head.ContainsKey key) -> new Table<'T> (((head.Add (key, value)) :: tail) :: this.List)
        | _ -> invalidArg key $"The key {key} already exists in the table"
    member this.Enter () =
        new Table<'T> ((Map.empty :: this.List.Head) :: this.List)
    member this.Exit count =
        new Table<'T> (this.List.Head.GetSlice (Some count, None) :: this.List)
    member this.Contains key =
        this.List.Head
        |> List.exists (fun block ->
            block |> Map.containsKey key)
    member this.Item
        with get(key) =
            this.List.Head
            |> List.find (fun block ->
                block |> Map.containsKey key)
            |> Map.find key
    member this.Pop 
        with get() =
            match entries with
            | head :: tail -> new Table<'T> (tail)
            | [] -> new Table<'T> ([])

type AccessType =
    Value | Reference

type StorageType =
    Dynamic | Static

type Symbol<'T> =
    | Global of Type : string * AccessType : AccessType * StorageType : StorageType * Value : 'T
    | Function of Type : string
    | Local of Type : string * AccessType : AccessType * StorageType : StorageType * Position : int * Value : 'T
    | Parameter of Type : string * AccessType : AccessType * StorageType : StorageType * Position : int
    | Member of Type : string * AccessType : AccessType * StorageType : StorageType * Offset : int * Value : 'T
    | Definition of AccessType : AccessType * StorageType : StorageType * Size : int * Value : 'T 
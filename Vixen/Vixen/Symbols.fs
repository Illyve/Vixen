module Symbols

type Table<'T> (entries : Map<string, 'T> list) =
    member private this.list = 
        match entries with
        | [] -> [ Map.empty ]
        | _ -> entries
    member this.Add (key, value) = 
        match this.list with
        | head :: tail when not (head.ContainsKey key) -> new Table<'T> ((head.Add (key, value)) :: tail)
        | _ -> invalidArg key $"The key {key} already exists in the table"
    member this.Enter () =
        new Table<'T> (Map.empty :: this.list)
    member this.Exit count =
        new Table<'T> (this.list.GetSlice (Some count, None))
    member this.Contains key =
        this.list
        |> List.exists (fun block ->
            block |> Map.containsKey key)
    member this.Item
        with get(key) =
            this.list
            |> List.find (fun block ->
                block |> Map.containsKey key)
            |> Map.find key

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
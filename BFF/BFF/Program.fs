open System

type Instruction =
    | Next
    | Previous
    | Increment
    | Decrement
    | Output
    | Input
    | StartLoop
    | EndLoop

let table : Map<char, Instruction> =
    [
        '<', Previous
        '>', Next
        '+', Increment
        '-', Decrement
        '.', Output
        ',', Input
        '[', StartLoop
        ']', EndLoop
    ]
    |> Map.ofList

let compile(code:string) : Instruction list =
    code.ToCharArray()
    |> Array.map(table.TryFind)
    |> Array.filter(Option.isSome)
    |> Array.map(Option.get)
    |> Array.toList

type Machine =
    {
        dataIndex : int
        data : Map<int, byte>
    }
    with
    static member previous(machine:Machine) : Machine =
        {machine with dataIndex = machine.dataIndex-1}
    static member next(machine:Machine) : Machine =
        {machine with dataIndex = machine.dataIndex+1}
    static member increment(machine:Machine) : Machine =
        {machine with 
            data = 
                machine.data 
                |> Map.add machine.dataIndex ((machine.data |> Map.tryFind machine.dataIndex |> Option.defaultValue 0uy) + 1uy )}
    static member decrement(machine:Machine) : Machine =
        {machine with 
            data = 
                machine.data 
                |> Map.add machine.dataIndex ((machine.data |> Map.tryFind machine.dataIndex |> Option.defaultValue 0uy) - 1uy )}
    static member output(machine:Machine) : unit =
        Console.Write($"{machine.data |> Map.tryFind machine.dataIndex |> Option.defaultValue 0uy |> char}")
    static member input(machine:Machine) : Machine =
        {machine with 
            data = 
                machine.data 
                |> Map.add machine.dataIndex (Console.ReadKey().KeyChar |> byte)}
    static member isLooping (machine:Machine) : bool =
        (machine.data |> Map.tryFind machine.dataIndex |> Option.defaultValue 0uy) <> 0uy


let rec advanceToEndOfLoop (counter:uint) (instructions: Instruction list) : Instruction list =
    match instructions with
    | [] ->
        raise (NotImplementedException())
    | [ head ] ->
        if head = Instruction.EndLoop && counter = 1u then
            []
        else
            raise (NotImplementedException())
    | head :: tail ->
        match head with
        | Instruction.EndLoop ->
            if counter = 1u then
                tail
            else
                advanceToEndOfLoop (counter-1u) tail
        | Instruction.StartLoop ->
            advanceToEndOfLoop (counter+1u) tail
        | _ ->
            advanceToEndOfLoop counter tail
            

let rec run (machine: Machine) (instructions: Instruction list) : Machine =
    match instructions with
    | [] ->
        machine
    | [ head ] ->
        match head with
        | Previous ->
            run (machine |> Machine.previous) []
        | Next->
            run (machine |> Machine.previous) []
        | Increment->
            run (machine |> Machine.previous) []
        | Decrement->
            run (machine |> Machine.previous) []
        | Output->
            machine |> Machine.output
            run machine []
        | Input->
            run (machine |> Machine.input) []
        | _ ->
            raise (NotImplementedException())
    | head :: tail ->
        match head with
        | Previous ->
            run (machine |> Machine.previous) tail
        | Next->
            run (machine |> Machine.next) tail
        | Increment->
            run (machine |> Machine.increment) tail
        | Decrement->
            run (machine |> Machine.decrement) tail
        | Output->
            machine |> Machine.output
            run machine tail
        | Input->
            run (machine |> Machine.input) tail
        | StartLoop->
            let mutable m = machine
            while m |> Machine.isLooping do
                m <- run m tail
            run m (tail |> advanceToEndOfLoop 1u)
        | EndLoop->
            machine

let code = ">++++++++[<+++++++++>-]<.>++++[<+++++++>-]<+.+++++++..+++.>>++++++[<+++++++>-]<++.------------.>++++++[<+++++++++>-]<+.<.+++.------.--------.>>>++++[<++++++++>-]<+."

[<EntryPoint>]
let main _ =
    code
    |> compile
    |> run {dataIndex=0; data=Map.empty}
    |> ignore
    Console.ReadLine() |> ignore
    0
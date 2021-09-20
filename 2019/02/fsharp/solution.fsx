open System.IO

let input = File.ReadAllText("../input.txt")

let parseMemory (input: string) = input.Split ',' |> Array.map int

type Instruction =
    | Halt
    | Add of x: int * y: int * dest: int
    | Mul of x: int * y: int * dest: int

type ExecutionResult =
    | Stop
    | Continue

type IntcodeCPU(memorySize: int) =
    let memory: int array = Array.zeroCreate memorySize
    let mutable pc = 0

    let fetchCurrentInstruction () =
        match memory.[pc] with
        | 99 -> Halt
        | 1 -> Add(memory.[pc + 1], memory.[pc + 2], memory.[pc + 3])
        | 2 -> Mul(memory.[pc + 1], memory.[pc + 2], memory.[pc + 3])
        | _ -> failwith $"Invalid instruction: {memory.[pc]}"

    let execute instruction =
        match instruction with
        | Halt -> Stop
        | Add (x, y, dest) ->
            let result = memory.[x] + memory.[y]
            Array.set memory dest result
            pc <- pc + 4
            Continue
        | Mul (x, y, dest) ->
            let result = memory.[x] * memory.[y]
            Array.set memory dest result
            pc <- pc + 4
            Continue

    let tick () = fetchCurrentInstruction () |> execute

    override _.ToString() =
        $"CPU {{ PC = {pc}, Memory = %A{memory} }}"

    member _.LoadProgram(newMemory: int array) =
        Array.blit newMemory 0 memory 0 (Array.length newMemory)
        pc <- 0

    member _.SetInputs(noun: int, verb: int) =
        Array.set memory 1 noun
        Array.set memory 2 verb

    member _.RunToHalt() =
        let rec loop result =
            match result with
            | Stop -> ()
            | Continue -> loop <| tick ()

        loop <| tick ()

    member _.Output = Array.head memory

let program = parseMemory input

let cpu = IntcodeCPU(Array.length program)

let runWithInput noun verb =
    cpu.LoadProgram(program)
    cpu.SetInputs(noun, verb)
    cpu.RunToHalt()
    cpu.Output

printfn "Part 1: %d" (runWithInput 12 2)

let inputProducesRequiredOutput (noun, verb) =
    let result = (runWithInput noun verb)
    result = 19690720

let noun, verb =
    Seq.allPairs (seq { 0 .. 99 }) (seq { 0 .. 99 })
    |> Seq.find inputProducesRequiredOutput

printfn "Part 2: %d" (100 * noun + verb)

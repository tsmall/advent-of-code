open System.IO

let fuelRequired mass =
    (mass / 3) - 2

let rec fuelRequiredRecursively masses =
    let fuels =
        masses
        |> Seq.map fuelRequired
        |> Seq.filter (fun n -> n > 0)
    if Seq.isEmpty fuels then fuels
    else Seq.concat [ fuels; fuelRequiredRecursively fuels ]

let masses =
    seq {
        for line in File.ReadLines("../input.txt") do
            int line
    }

let totalFuelForInitialMass =
    masses
    |> Seq.map fuelRequired
    |> Seq.sum

let totalFuelCountingFuel =
    masses
    |> fuelRequiredRecursively
    |> Seq.sum

printfn "Part 1: %d" totalFuelForInitialMass
printfn "Part 2: %d" totalFuelCountingFuel

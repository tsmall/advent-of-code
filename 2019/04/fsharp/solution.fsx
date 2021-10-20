let getDigits n =
    let ns = Array.zeroCreate 6
    let mutable n = n

    for pow = 5 downto 0 do
        let div = pown 10 pow
        let m = n / div
        Array.set ns (5 - pow) m
        n <- n - m * div

    ns

let hasSameAdjacentDigits digits =
    let pairs = Array.pairwise digits
    Array.exists (fun (x, y) -> x = y) pairs

let hasNoDecreasingDigits digits =
    let pairs = Array.pairwise digits
    Array.forall (fun (x, y) -> y >= x) pairs

let originalRequirements =
    [ hasSameAdjacentDigits
      hasNoDecreasingDigits ]

let hasPairOfDigits digits =
    digits
    |> Array.countBy id
    |> Array.exists (fun (n, count) -> count = 2)

let revisedRequirements =
    [ hasPairOfDigits
      hasNoDecreasingDigits ]

let meets requirements n =
    let digits = getDigits n
    List.forall (fun f -> f digits) requirements

let rangeMin = 240298
let rangeMax = 784956

let allPossiblePasswords = seq { rangeMin .. rangeMax }

let originalPasswords =
    allPossiblePasswords
    |> Seq.filter (meets originalRequirements)

let revisedPasswords =
    allPossiblePasswords
    |> Seq.filter (meets revisedRequirements)

printfn $"Part 1: %d{Seq.length originalPasswords}"
printfn $"Part 2: %d{Seq.length revisedPasswords}"

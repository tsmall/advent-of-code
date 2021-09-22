open System
open System.IO

module Point =
    type T = int * int

    let origin = (0, 0)

    let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

type Direction =
    | Right
    | Left
    | Up
    | Down

type Command = { Direction: Direction; Count: int }

let parseCommand (s: string) =
    let direction =
        match s.[0] with
        | 'R' -> Right
        | 'L' -> Left
        | 'U' -> Up
        | 'D' -> Down
        | _ -> failwith $"Invalid command: {s}"

    let count = Int32.Parse(s.AsSpan().Slice(1))
    { Direction = direction; Count = count }

let parseCommands (s: string) =
    s.Split(',')
    |> Array.map parseCommand
    |> Array.toList

let pointsInLine startPoint command =
    let { Count = count; Direction = direction } = command

    let delta =
        match direction with
        | Right -> (1, 0)
        | Left -> (-1, 0)
        | Up -> (0, 1)
        | Down -> (0, -1)

    let nextPoint point _ = Point.add point delta
    List.scan nextPoint startPoint [ 1 .. count ]

let pointsInWire commands =
    let nextWire previousPoints command =
        let startPoint = List.last previousPoints
        pointsInLine startPoint command |> List.tail

    List.scan nextWire [ Point.origin ] commands
    |> List.concat

let intersectingPoints wire1Points wire2Points =
    Set.intersect (Set.ofList wire1Points) (Set.ofList wire2Points)
    |> Set.remove (0, 0)

let manhattanDistance (x, y) = abs x + abs y

let findClosestIntersection intersections =
    intersections
    |> Set.map manhattanDistance
    |> Set.minElement

let stepsToPoint wirePoints point =
    List.takeWhile ((<>) point) wirePoints
    |> List.length

let findShortestIntersection intersections wire1Points wire2Points =
    intersections
    |> Set.map (fun p -> stepsToPoint wire1Points p + stepsToPoint wire2Points p)
    |> Set.minElement

let input = File.ReadAllText("../input.txt")

let lines = input.Split('\n')

let wire1Points =
    lines.[0] |> parseCommands |> pointsInWire

let wire2Points =
    lines.[1] |> parseCommands |> pointsInWire

let intersections =
    intersectingPoints wire1Points wire2Points

printfn "Part 1: %d"
<| findClosestIntersection intersections

printfn "Part 2: %d"
<| findShortestIntersection intersections wire1Points wire2Points

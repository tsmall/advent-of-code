import Foundation

struct Sub {
    var position = 0
    var depth = 0
    var aim = 0

    func move(step: Step) -> Sub {
        var s = self

        switch step {
        case .forward(let amount):
            s.position += amount
            s.depth += s.aim * amount
        case .up(let amount):
            s.aim -= amount
        case .down(let amount):
            s.aim += amount
        }

        return s
    }
}

enum Step {
    case forward(Int)
    case up(Int)
    case down(Int)

    init?(_ s: Substring) {
        let parts = s.split(separator: " ")
        let amount = Int(parts[1])!

        switch parts[0] {
        case "forward":
            self = .forward(amount)
        case "up":
            self = .up(amount)
        case "down":
            self = .down(amount)
        default:
            return nil
        }
    }
}

let input = try! String(contentsOfFile: "../input.txt")
let lines = input.split(separator: "\n")
let steps = lines.map { Step($0)! }

let sub = steps.reduce(Sub(), { sub, step in sub.move(step: step) })

print("Part 1: \(sub.position * sub.aim)")
print("Part 2: \(sub.position * sub.depth)")

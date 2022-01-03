import Foundation

let input = try! String(contentsOfFile: "../input.txt")
let depths = input.split(separator: "\n").map { Int($0)! }

var incs = 0
var sums = 0
for curr in 1..<depths.count {
    if depths[curr-1] < depths[curr] {
        incs += 1
    }

    if curr >= 3 {
        let a = depths[curr-3] + depths[curr-2] + depths[curr-1]
        let b = depths[curr-2] + depths[curr-1] + depths[curr]
        if b > a {
            sums += 1
        }
    }
}

print("Part 1: \(incs)")
print("Part 2: \(sums)")

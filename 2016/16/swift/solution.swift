let generator = DragonCurveDataGenerator()

// Part 1
generator.load(data: "01000100010010111")
generator.generateToFillDisk(sized: 272)
print("Part 1: \(generator.checksum())")

// Part 2
generator.load(data: "01000100010010111")
generator.generateToFillDisk(sized: 35651584)
print("Part 2: \(generator.checksum())")


class DragonCurveDataGenerator {
    var dataBuffer: [Character] = []

    func load(data: String) {
        dataBuffer.removeAll(keepingCapacity: true)
        dataBuffer.append(contentsOf: data)
    }

    func generateToFillDisk(sized size: Int) {
        dataBuffer.reserveCapacity(size * 2)
        while dataBuffer.count < size {
            let originalReversed = dataBuffer.reversed()
            dataBuffer.append("0")
            for c in originalReversed {
                dataBuffer.append(c == "0" ? "1" : "0")
            }
        }
        dataBuffer.removeLast(dataBuffer.count - size)
    }

    func checksum() -> String {
        var data: [Character] = Array(dataBuffer)
        var buffer: [Character] = []
        buffer.reserveCapacity(data.count)

        while data.count % 2 == 0 {
            buffer.removeAll(keepingCapacity: true)
            for i in stride(from: 0, to: data.count - 1, by: 2) {
                let a = data[i]
                let b = data[i + 1]
                buffer.append(a == b ? "1" : "0")
            }

            let tmp = buffer
            buffer = data
            data = tmp
        }

        return String(data)
    }
}

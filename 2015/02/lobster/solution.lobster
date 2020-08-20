import std

class box:
    length: int
    width: int
    height: int

def areas(b:box):
    return [
        b.length * b.width,
        b.width * b.height,
        b.height * b.length
    ]

def perimeters(box):
    return [
        2 * (box.length + box.width),
        2 * (box.width + box.height),
        2 * (box.height + box.length)
    ]

def volume(box):
    return box.length * box.width * box.height

def paper_needed(box):
    let a = areas(box)
    let s = min(a)
    return s + (2 * sum(a))

def ribbon_needed(box):
    let p = min(perimeters(box))
    return p + box.volume

def sum(numbers):
    return fold(numbers, 0):
        _a + _b

def parse(s:string, b:box):
    let parts = tokenize(s, "x", " ")
    let nums = map(parts): string_to_int(_)
    b.length = nums[0]
    b.width = nums[1]
    b.height = nums[2]

enum result:
    ok
    done

def next(box):
    let input = get_line("")
    if input.length:
        parse(input, box)
        return ok
    else:
        return done

var total_paper = 0
var total_ribbon = 0

let box = box{0, 0, 0}
while next(box) != done:
    total_paper += box.paper_needed()
    total_ribbon += box.ribbon_needed()

print "Part 1: " + total_paper
print "Part 2: " + total_ribbon

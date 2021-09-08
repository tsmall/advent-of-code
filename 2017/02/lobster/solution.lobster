import std

let input = read_file("../input.txt", true)
if input == nil:
    fatal_exit("Could not read input file")

def next_int(str, i):
    var chars = 0
    var num = 0
    while str[i] >= '0' and str[i] <= '9':
        num *= 10
        num += str[i] - '0'
        i += 1
        chars += 1
    return num, chars

def for_int(str, f):
    var i = 0
    while i < str.length:
        let num, offset = next_int(str, i)
        if offset > 0:
            f(num)
        i += offset + 1

def to_nums(row):
    let nums = vector_reserve(typeof return, 20)
    for_int(row): nums.push(_)
    return nums

def find_evenly_divisible_nums(nums):
    var i = 0
    while i < nums.length:
        var j = i + 1
        while j < nums.length:
            if nums[i] % nums[j] == 0:
                return nums[i], nums[j]
            elif nums[j] % nums[i] == 0:
                return nums[j], nums[i]
            j += 1
        i += 1
    fatal_exit("No evenly divisible numbers found in: " + nums)

let rows = tokenize(input, "\n", "")
var minmax_checksum = 0
var even_checksum = 0
for(rows) row:
    let nums = to_nums(row)
    minmax_checksum += max(nums) - min(nums)
    let n, m = find_evenly_divisible_nums(nums)
    even_checksum += n / m

print "Part 1: " + minmax_checksum
print "Part 2: " + even_checksum

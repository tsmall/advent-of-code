import std

struct byr:
    val:int

struct iyr:
    val:int

struct eyr:
    val:int

enum dim:
    cm
    in

struct hgt:
    val:int
    dim:dim

struct hcl:
    val:string?

struct ecl:
    val:string?

struct pid:
    val:string?

class passport:
    byr = byr { 0 }
    iyr = iyr { 0 }
    eyr = eyr { 0 }
    hgt = hgt { 0, cm }
    hcl = hcl { nil }
    ecl = ecl { nil }
    pid = pid { nil }
    cid:string? = nil

def each_line(fun):
    var line = ""
    var empty_lines = 0
    while(line = get_line("")):
        if line.length == 0:
            empty_lines++
            if empty_lines > 1:
                return
        else:
            empty_lines = 0
        fun(line)

def update(p:passport, pair):
    let field = pair[0]
    let value = pair[1]
    switch field:
        case "byr": p.byr = byr { string_to_int(value, 10) }
        case "iyr": p.iyr = iyr { string_to_int(value, 10) }
        case "eyr": p.eyr = eyr { string_to_int(value, 10) }
        case "hcl": p.hcl = hcl { value }
        case "ecl": p.ecl = ecl { value }
        case "pid": p.pid = pid { value }
        case "cid": p.cid = value
        case "hgt":
            var dim = in
            if find_string_reverse(value, "cm") != -1:
                dim = cm
            p.hgt = hgt { string_to_int(value, 10), dim }

def each_passport(fun):
    var p = passport {}
    each_line() line:
        if line.length == 0:
            fun(p)
            p = passport {}
        else:
            let pairs = tokenize(line, " ", " ")
            for(pairs) pair:
                p.update(tokenize(pair, ":", " "))

def has_required_fields(p::passport):
    return byr.val != 0 and iyr.val != 0   and eyr.val != 0   and
           hgt.val != 0 and hcl.val != nil and ecl.val != nil and
           pid.val != nil

def is_valid(p::passport):
    if not p.has_required_fields: return false
    return byr.is_valid and iyr.is_valid and eyr.is_valid and
           hgt.is_valid and hcl.is_valid and ecl.is_valid and
           pid.is_valid

def is_valid(byr::byr):
    return val >= 1920 and val <= 2002

def is_valid(iyr::iyr):
    return val >= 2010 and val <= 2020

def is_valid(eyr::eyr):
    return val >= 2020 and val <= 2030

def is_valid(hgt::hgt):
    switch dim:
        case cm: return val >= 150 and val <= 193
        case in: return val >= 59  and val <= 76
    fatal_exit("Unable to validate hgt")

def is_valid(hcl::hcl):
    var length = 0
    if val == nil: return false
    if val[0] != '#': return false
    for (substring(val, 1, -1)) char:
        if not is_hex(char): return false
        if ++length > 6: return false
    return true

def is_hex(char):
    return (char >= '0' and char <= '9') or
           (char >= 'a' and char <= 'f')

def is_valid(ecl::ecl):
    if val == nil: return false
    let allowed = [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]
    return exists(allowed): _ == val

def is_valid(pid::pid):
    if val == nil: return false
    return val.length == 9 and all_match(val): _ >= '0' and _ <= '9'

def all_match(xs, fun):
    for(xs) x:
        if not fun(x): return false
    return true

var required_field_count = 0
var valid_count = 0
each_passport() p:
    if p.has_required_fields:
        required_field_count++
    if p.is_valid:
        valid_count++

print "Part 1: " + required_field_count
print "Part 2: " + valid_count

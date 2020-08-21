import set

let move_north = xy_i {  0,  1 }
let move_east  = xy_i {  1,  0 }
let move_south = xy_i {  0, -1 }
let move_west  = xy_i { -1,  0 }
let move_noop  = xy_i {  0,  0 }

def parse(char):
    return switch char:
        case '^': move_north
        case '>': move_east
        case 'v': move_south
        case '<': move_west
        default: move_noop

class actor:
    position:xy_i = xy_i { 0, 0 }

class pair:
    santa:actor
    robot:actor
    current:actor

    def swap():
        if current == santa:
            current = robot
        else:
            current = santa

    def move(movement):
        current.position += movement

def make_pair():
    let santa = actor {}
    let robot = actor {}
    return pair { santa: santa, robot: robot, current: santa }


let solo_visits = set<xy_i> { 1009 }
var solo_position = xy_i { 0, 0 }
solo_visits.add(solo_position)

let pair_visits = set<xy_i> { 1009 }
let pair = make_pair()
pair_visits.add(pair.current.position)

var input = ""
while (input = get_line("")).length:
    for(input) c:
        let movement = parse(c)
        solo_position += movement
        solo_visits.add(solo_position)
        pair.move(movement)
        pair_visits.add(pair.current.position)
        pair.swap()

print "Part 1: " + solo_visits.cardinality
print "Part 2: " + pair_visits.cardinality

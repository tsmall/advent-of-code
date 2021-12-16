# Advent of Code 2021
# Day 15: Chiton

module Day15
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    map = parse(text)
    solve(map, 1)
  end

  def part_two(text=nil)
    text ||= input
    map = parse(text)
    solve(map, 5)
  end

  def solve(map, size)
    w = map[0].size
    h = map.size

    cost = Array.new(h*size) { Array.new(w*size, Float::INFINITY) }
    cost[0][0] = 0

    10.times do
      (0..h*size-1).each do |y|
        (0..w*size-1).each do |x|
          next if x == 0 and y == 0
          l = x > 0        ? cost[y][x-1] : Float::INFINITY
          r = x < w*size-1 ? cost[y][x+1] : Float::INFINITY
          u = y > 0        ? cost[y-1][x] : Float::INFINITY
          d = y < h*size-1 ? cost[y+1][x] : Float::INFINITY
          val = (map[y%h][x%w] + x/w + y/h - 1) % 9 + 1
          cost[y][x] = val + [l, r, u, d].min
        end
      end
    end

    cost[h*size-1][w*size-1]
  end

  def parse(text)
    text.each_line.collect do |line|
      line.strip.each_char.collect(&:to_i)
    end
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day15.run
end

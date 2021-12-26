# Advent of Code 2021
# Day 25: Sea Cucumber

module Day25
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    solve(text)
  end

  def part_two(text=nil)
    text ||= input
    0
  end

  def solve(text)
    grid = text.strip.lines.collect(&:strip)
    rows = grid.size
    cols = grid.first.size

    ticks = 0
    no_moves = false
    until no_moves
      ticks += 1
      no_moves = true

      copy = grid.collect(&:clone)
      rows.times do |y|
        cols.times do |x|
          if grid[y][x] == '>' and grid[y][(x+1)%cols] == '.'
            no_moves = false

            copy[y][x] = '.'
            copy[y][(x+1)%cols] = '>'
          end
        end
      end

      grid = copy.collect(&:clone)
      rows.times do |y|
        cols.times do |x|
          if copy[y][x] == 'v' and copy[(y+1)%rows][x] == '.'
            no_moves = false

            grid[y][x] = '.'
            grid[(y+1)%rows][x] = 'v'
          end
        end
      end
    end

    ticks
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day25.run
end

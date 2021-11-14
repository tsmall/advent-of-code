module Extensions
  refine String do
    def up?
      self == '('
    end

    def down?
      self == ')'
    end
  end
end

module Day01
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one
    floor = 0

    input.each_char do |char|
      floor += step(char)
    end

    floor
  end

  def part_two
    floor = 0
    steps = 0

    input.each_char do |char|
      floor += step(char)
      steps += 1

      if floor < 0
        break
      end
    end

    steps
  end

  def input
    File.open("../input.txt")
  end

  def step(direction)
    if direction.up?
      1
    elsif direction.down?
      -1
    else
      0
    end
  end
end

if $PROGRAM_NAME == __FILE__
  Day01.run
end

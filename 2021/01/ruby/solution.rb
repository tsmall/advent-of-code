# Advent of Code 2021
# Day 1: Sonar Sweep

module Day01
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one
    count_increases(depths)
  end

  def part_two
    count_increases(sliding_sums(depths))
  end

  def count_increases(numbers)
    numbers.lazy
      .each_cons(2)
      .select { |x, y| x < y }
      .count
  end

  def sliding_sums(numbers)
    numbers.lazy
      .each_cons(3)
      .collect(&:sum)
  end

  def depths
    input.each_line.map(&:to_i)
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day01.run
end

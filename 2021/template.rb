# Advent of Code 2021
# Day X: TODO

module DayX
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    0
  end

  def part_two(text=nil)
    text ||= input
    0
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  DayX.run
end

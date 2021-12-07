# Advent of Code 2021
# Day 6: Lanternfish

require 'immutable'

module Day06
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    run_simulation(text, 80)
  end

  def part_two(text=nil)
    text ||= input.read
    run_simulation(text, 256)
  end

  def run_simulation(text, days)
    school = parse_school(text)
    final_school = simulate(school, days)
    final_school.values.sum
  end

  def simulate(school, days)
    days.times.inject(school) do |school, day|
      tick(school)
    end
  end

  def tick(school)
    Immutable::Hash[
      8 => school[0],
      7 => school[8],
      6 => school[7] + school[0],
      5 => school[6],
      4 => school[5],
      3 => school[4],
      2 => school[3],
      1 => school[2],
      0 => school[1],
    ]
  end

  def parse_school(text)
    school = Immutable::Hash.new { 0 }
    numbers = text.split(',').map(&:to_i)
    numbers.inject(school) do |s, n|
      s.put(n, &:next)
    end
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day06.run
end

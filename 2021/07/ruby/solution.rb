# Advent of Code 2021
# Day 7: The Treachery of Whales

require 'immutable'

module Day07
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    positions = parse(text)
    most_efficient_dest(positions, method(:naive_fuel_cost))
  end

  def part_two(text=nil)
    text ||= input.read
    positions = parse(text)
    most_efficient_dest(positions, method(:actual_fuel_cost))
  end

  def most_efficient_dest(positions, fuel_proc)
    costs = {}

    start = median(positions)
    1000.times do |diff|
      dest = start + diff
      costs[dest] = total_fuel_cost(positions, dest, fuel_proc)

      dest = start - diff
      if dest > 0
        costs[dest] = total_fuel_cost(positions, dest, fuel_proc)
      end
    end

    costs.values.min
  end

  def total_fuel_cost(positions, dest, fuel_proc)
    costs = fuel_costs(positions, dest, fuel_proc)
    costs.sum
  end

  def fuel_costs(positions, dest, fuel_proc)
    positions.collect do |src|
      fuel_proc.call(src, dest)
    end
  end

  def naive_fuel_cost(src, dest)
    distance(src, dest)
  end

  def actual_fuel_cost(src, dest)
    n = distance(src, dest)
    sum_up_to(n)
  end

  def distance(src, dest)
    (src - dest).abs
  end

  def sum_up_to(n)
    (n * (n + 1)) / 2
  end

  def median(positions)
    positions
      .group_by { |pos| pos }
      .values
      .max_by(&:size)
      .first
  end

  def parse(text)
    Immutable::Vector.new(text.split(',').collect(&:to_i))
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day07.run
end

# Advent of Code 2021
# Day 5: Hydrothermal Venture

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end
end

module Day05
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input

    lines = parse_lines(text)
    valid_lines = lines.select do |line|
      horizontal?(line) or vertical?(line)
    end

    overlapping_points(valid_lines).size
  end

  def part_two(text=nil)
    text ||= input
    lines = parse_lines(text)
    overlapping_points(lines).size
  end

  def overlapping_points(lines)
    points = points_in_lines(lines)

    # Using the Immutable library for these computations was slower than I liked.
    # It took over 18 seconds to do this computation using Immutable collections,
    # but close to 6 when using Ruby's mutable collections.
    #
    # And since all this mutation is internal,
    # this still presents an immutable interface.
    # Win-win!
    bag = Hash.new { 0 }
    points.each do |p|
      bag[p] += 1
    end

    bag
      .select { |k, v| v > 1 }
      .keys
  end

  def points_in_lines(lines)
    lines
      .collect { |l| points_in_line(l) }
      .flatten(1)
      .to_list
  end

  def points_in_line(line)
    line => {start: {x: x1, y: y1}, end: {x: x2, y: y2}}

    if horizontal?(line)
      xs = abs_range(x1, x2)
      ys = [y1] * xs.size
      xys = Immutable::Set.new(xs.zip(ys))
    elsif vertical?(line)
      ys = abs_range(y1, y2)
      xs = [x1] * ys.size
      xys = Immutable::Set.new(xs.zip(ys))
    else
      xd = if x1 < x2 then 1 else -1 end
      yd = if y1 < y2 then 1 else -1 end

      x = x1 - xd
      y = y1 - yd

      # Since the problem states they're always on 45 degree angles,
      # it doesn't matter which range we choose for this.
      xys = abs_range(x1, x2).inject(Immutable::Set[]) do |points, _|
        points << [x += xd, y += yd]
      end
    end

    xys.collect { |x, y| Immutable::Hash[x: x, y: y] }
  end

  def abs_range(i, j)
    if i < j
      i..j
    else
      j..i
    end
  end

  def horizontal?(line)
    line => {start: {y: y1}, end: {y: y2}}
    y1 == y2
  end

  def vertical?(line)
    line => {start: {x: x1}, end: {x: x2}}
    x1 == x2
  end

  def parse_lines(text)
    text
      .each_line
      .collect { |line| parse_line(line.strip) }
  end

  def parse_line(line)
    regex = /(?<x1>\d+),(?<y1>\d+) -> (?<x2>\d+),(?<y2>\d+)/
    match = regex.match(line)

    x1 = match[:x1].to_i
    y1 = match[:y1].to_i
    x2 = match[:x2].to_i
    y2 = match[:y2].to_i

    Immutable::Hash[
      start: Immutable::Hash[x: x1, y: y1],
      end: Immutable::Hash[x: x2, y: y2],
    ]
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day05.run
end

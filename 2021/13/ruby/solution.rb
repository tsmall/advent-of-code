# Advent of Code 2021
# Day 13: Transparent Origami

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end
end

module Day13
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    parse(text) => {grid:, folds:}
    folded = fold_grid(grid, **folds.first)
    folded.size
  end

  def part_two(text=nil)
    text ||= input.read
    parse(text) => {grid:, folds:}
    final = folds.inject(grid) { |grid, fold| fold_grid(grid, **fold) }
    render(final)
  end

  def render(grid)
    x = grid.collect { |point| point[:x] }.max
    y = grid.collect { |point| point[:y] }.max

    grid_vector(x, y).each do |row|
      puts row.collect { |p| grid.include?(p) ? '#' : '.' }.join
    end

    nil
  end

  def grid_vector(max_x, max_y)
    (0..max_y).collect do |y|
      (0..max_x).collect do |x|
        Immutable::Hash[x: x, y: y]
      end
    end
  end

  def fold_grid(grid, axis:, position:)
    grid.collect do |point|
      if point[axis] > position
        flip_point(point, axis: axis, position: position)
      else
        point
      end
    end
  end

  def flip_point(point, axis:, position:)
    point.put(axis) { |v| position - (v - position) }
  end

  def parse(text)
    points_section, folds_section = text.split("\n\n")

    points = points_section.each_line.collect do |line|
      x, y = line.strip.split(',').collect(&:to_i)
      Immutable::Hash[x: x, y: y]
    end

    fold_re = /fold along (?<axis>x|y)=(?<position>\d+)/
    folds = folds_section.each_line.collect do |line|
      match = fold_re.match(line)
      Immutable::Hash[
        axis: match[:axis].to_sym,
        position: match[:position].to_i,
      ]
    end

    Immutable::Hash[
      grid: Immutable::Set.new(points),
      folds: Immutable::Vector.new(folds),
    ]
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day13.run
end

# Advent of Code 2021
# Day 11: Dumbo Octopus

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end

  refine Immutable::Vector do
    def deconstruct
      to_a
    end
  end
end

module Day11
  module_function
  using Extensions

  JUST_FLASHED = -1

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    grid = parse(text)
    result = run_to_count(grid, 100)
    result[:flashes]
  end

  def part_two(text=nil)
    text ||= input
    grid = parse(text)
    result = run_to_synchronized_flash(grid)
    result[:ticks]
  end

  def run_to_count(grid, tick_count)
    all_ticks(grid)
      .drop(tick_count - 1)
      .take(1)
      .first
  end

  def run_to_synchronized_flash(grid)
    all_ticks(grid).find { |state| all_flashed?(state[:grid]) }
  end

  def all_ticks(grid)
    state = tick(grid).put(:ticks, 1)
    Immutable.iterate(state) do |state|
      state => {grid:, flashes:, ticks:}
      tick(grid) => {grid: new_grid, flashes: new_flashes}
      Immutable::Hash[grid: new_grid, flashes: flashes + new_flashes, ticks: ticks + 1]
    end
  end

  def tick(grid)
    increased = increase_all(grid)
    resolve_flashes(increased) => {grid: resolved, flashes:}
    Immutable::Hash[grid: zero_out(resolved), flashes: flashes]
  end

  def zero_out(grid)
    grid.collect do |row|
      row.collect { |octopus| octopus == JUST_FLASHED ? 0 : octopus }
    end
  end

  def all_flashed?(grid)
    grid.all? { |row| row.all?(&:zero?) }
  end

  def resolve_flashes(grid, total_flashes = 0)
    flashes = find_all_flash_points(grid)

    if flashes.empty?
      return Immutable::Hash[grid: grid, flashes: total_flashes]
    else
      new_grid = flashes.inject(grid) { |grid, point| flash(grid, point) }
      return resolve_flashes(new_grid, total_flashes + flashes.size)
    end
  end

  def find_all_flash_points(grid)
    grid.each_with_index.inject(Immutable::Vector.empty) do |points, indexed_row|
      indexed_row => [row, y]

      row.each_with_index.inject(points) do |points, indexed_cols|
        indexed_cols => [octopus, x]

        if octopus > 9
          points.push(Immutable::Vector[x, y])
        else
          points
        end
      end
    end
  end

  def flash(grid, point)
    flashed = neighbors(grid, point).inject(grid) do |grid, neighbor|
      neighbor => [x, y]
      grid.update_in(y, x) { |octopus| increase(octopus) }
    end

    point => [x, y]
    flashed.update_in(y, x) { JUST_FLASHED }
  end

  def neighbors(grid, point)
    point => [x, y]
    possible = Immutable::Vector[
      Immutable::Vector[x - 1, y - 1], Immutable::Vector[x, y - 1], Immutable::Vector[x + 1, y - 1],
      Immutable::Vector[x - 1, y],                                  Immutable::Vector[x + 1, y],
      Immutable::Vector[x - 1, y + 1], Immutable::Vector[x, y + 1], Immutable::Vector[x + 1, y + 1],
    ]

    max_y = grid.size
    max_x = grid.first.size
    possible.reject { |x, y| x < 0 or x >= max_x or y < 0 or y >= max_y }
  end

  def increase_all(grid)
    grid.collect do |row|
      row.collect { |octopus| increase(octopus) }
    end
  end

  def increase(octopus)
    if octopus == JUST_FLASHED
      JUST_FLASHED
    else
      octopus + 1
    end
  end

  def parse(text)
    Immutable.from(
      text.each_line.collect do |line|
        line.strip.each_char.collect(&:to_i)
      end
    )
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day11.run
end

# Advent of Code 2021
# Day 9: Smoke Basin

module Day09
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    heightmap = parse(text)
    low_points = low_points(heightmap)
    risk_levels = low_points.collect { |record| risk_level(record) }
    risk_levels.sum
  end

  def part_two(text=nil)
    text ||= input
    heightmap = parse(text)
    basins = basins(heightmap)

    basins.sort_by(&:size).reverse.take(3).inject(1) do |product, basin|
      product * basin.size
    end
  end

  def risk_level(record)
    record[:depth] + 1
  end

  def basins(heightmap)
    low_points = low_points(heightmap)
    low_points.collect do |record|
      find_basin(record)
    end
  end

  def find_basin(record, seen=[])
    record => {depth:, neighbors:}
    return [] if depth == 9

    seen = seen.append(record)
    basin = [depth]

    neighbors.each do |neighbor|
      if not seen.include?(neighbor)
        basin.concat(find_basin(neighbor, seen))
      end
    end

    basin
  end

  def low_points(heightmap)
    heightmap.select do |record|
      low_point?(record)
    end
  end

  def low_point?(record)
    record => {depth:, neighbors:}
    neighbor_depths = neighbors.collect { |n| n[:depth] }
    neighbor_depths.all? { |nd| nd > depth }
  end

  def parse(text)
    records = []

    prev_row = []
    curr_row = []

    text.each_char do |char|
      if char == "\n"
        records.concat(curr_row)
        prev_row = curr_row
        curr_row = []
        next
      end

      depth = char.to_i
      record = {depth: depth, neighbors: []}

      if not curr_row.empty?
        prev = curr_row.last
        prev[:neighbors] << record
        record[:neighbors] << prev
      end

      above = prev_row[curr_row.size]
      if above
        above[:neighbors] << record
        record[:neighbors] << above
      end

      curr_row << record
    end

    records
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day09.run
end

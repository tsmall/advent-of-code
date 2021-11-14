class Box
  attr_reader :length
  attr_reader :width
  attr_reader :height

  def self.from_str(str)
    dimensions = str.split('x').collect(&:to_i)
    self.new(dimensions)
  end

  def initialize(dimensions)
    (@length, @width, @height) = dimensions
  end

  def paper_needed
    surface_area + areas.min
  end

  def ribbon_needed
    perimeters.min + volume
  end

  def surface_area
    2 * areas.sum
  end

  def volume
    length * width * height
  end

  def areas
    [length * width, width * height, height * length]
  end

  def perimeters
    [2 * (length + width), 2 * (width + height), 2 * (height + length)]
  end
end

module Day02
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one
    boxes.inject(0) { |total, box| total + box.paper_needed }
  end

  def part_two
    boxes.inject(0) { |total, box| total + box.ribbon_needed }
  end

  def boxes
    $boxes ||= load_boxes
  end

  def load_boxes
    input = File.open('../input.txt')
    lines = input.readlines
    lines.collect do |line|
      Box.from_str(line)
    end
  end
end

if $PROGRAM_NAME == __FILE__
  Day02.run
end

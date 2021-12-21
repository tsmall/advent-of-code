# Advent of Code 2021
# Day 20: Trench Map

module Extensions
  refine String do
    def to_binary
      case self
      when '#'
        1
      when '.'
        0
      else
        raise "Unexpected character encountered during binary conversion: #{self}"
      end
    end
  end

  refine Numeric do
    def to_bchar
      case self
      when 1
        '#'
      when 0
        '.'
      else
        raise "Unexpected number encountered during binary-to-char conversion: #{self}"
      end
    end
  end
end

module Day20
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    image = parse(text)
    2.times { image.enhance }
    image.lit_pixel_count
  end

  def part_two(text=nil)
    text ||= input.read
    image = parse(text)
    50.times { image.enhance }
    image.lit_pixel_count
  end

  def parse(text)
    algorithm, pixels = text.split("\n\n")

    algorithm = algorithm.strip.each_char.collect(&:to_binary)
    pixels = pixels.each_line.collect do |line|
      line.strip.each_char.collect(&:to_binary)
    end

    Image.new(pixels: pixels, algorithm: algorithm)
  end

  def input
    File.open("../input.txt")
  end
end

class Image
  using Extensions
  attr_reader :pixels, :algorithm, :rest

  NEIGHBORS = [-1, 0, 1].flat_map do |y|
    [-1, 0, 1].collect do |x|
      [x, y]
    end
  end

  def initialize(pixels:, algorithm:)
    @pixels = pixels
    @algorithm = algorithm
    @rest = 0
  end

  def width
    pixels.first.size
  end

  def height
    pixels.size
  end

  def enhance
    buffer = []

    (-1..height+1).each do |y|
      row = []
      buffer << row

      (-1..width+1).each do |x|
        row << new_value(x: x, y: y)
      end
    end

    @pixels = buffer

    @rest =
      if rest == 0
        algorithm[0b000000000]
      else
        algorithm[0b111111111]
      end

    nil
  end

  def lit_pixel_count
    count = 0
    pixels.each do |row|
      count += row.count(1)
    end
    count
  end

  def to_s
    pixels.each do |row|
      puts row.map(&:to_bchar).join
    end

    nil
  end

  def new_value(x:, y:)
    xys  = neighbors_of(x: x, y: y)
    vals = lookup(xys)
    i    = from_binary_list(vals)

    algorithm[i]
  end

  def neighbors_of(x:, y:)
    NEIGHBORS.collect do |xd, yd|
      [x + xd, y + yd]
    end
  end

  def lookup(xys)
    xys.collect do |x, y|
      if x < 0 or x >= width or y < 0 or y >= height
        rest
      else
        pixels[y][x]
      end
    end
  end

  def from_binary_list(ns)
    ns.join.to_i(2)
  end
end

if $PROGRAM_NAME == __FILE__
  Day20.run
end

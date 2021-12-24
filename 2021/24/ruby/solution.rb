# Advent of Code 2021
# Day 24: Arithmetic Logic Unit

module Day24
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    a, b = parse(text)
    calculate(a, b, minimize: false)
  end

  def part_two(text=nil)
    text ||= input
    a, b = parse(text)
    calculate(a, b, minimize: true)
  end

  def calculate(a, b, minimize:)
    digits = [0] * 14
    stack = []

    (0..13).each do |i|
      if a[i] > 0
        stack << [i, b[i]]
      else
        ix, v = stack.pop()
        v += a[i]

        if minimize
          digits[ix] = 1 - [0, v].min
          digits[i]  = 1 + [0, v].max
        else
          digits[ix] = 9 - [0, v].max
          digits[i]  = 9 + [0, v].min
        end
      end
    end

    digits.inject(0) { |total, n| total * 10 + n }
  end

  def parse(text)
    a = []
    b = []

    text.each_line.each_with_index do |line, i|
      a << line.split.last.to_i if i % 18 ==  5
      b << line.split.last.to_i if i % 18 == 15
    end

    [a, b]
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day24.run
end

# Advent of Code 2021
# Day 18: Snailfish

require 'immutable'

module Day18
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    numbers = parse(text)
    answer = sum(numbers)
    magnitude(answer)
  end

  def part_two(text=nil)
    text ||= input
    numbers = parse(text)
    magnitudes = numbers.permutation(2).collect do |number1, number2|
      sum = add(number1, number2)
      magnitude(sum)
    end
    magnitudes.max
  end

  def magnitude(number)
    calc = Proc.new do |n|
      if n.class == Array
        l, r = n
        3*calc.call(l) + 2*calc.call(r)
      else
        n
      end
    end

    a = unparse(number)
    calc.call(a)
  end

  def sum(numbers)
    numbers.inject { |number1, number2| add(number1, number2) }
  end

  def add(number1, number2)
    added = (number1 + number2).collect do |each|
      each.put(:depth) { |depth| depth + 1 }
    end

    reduce(added)
  end

  def reduce(number)
    while true
      reduced = explode(number)
      if reduced != number
        number = reduced
        next
      end

      reduced = split(number)
      if reduced != number
        number = reduced
        next
      end

      break
    end

    number
  end

  def explode(number)
    (0..number.size-1).each do |i|
      l = number[i]
      r = number[i+1]

      if l[:depth] > 4 and l[:depth] == r[:depth]
        # Left
        if i > 0
          number = number.update_in(i-1, :value) { |v| v + l[:value] }
        end

        # Right
        if i+2 < number.size
          number = number.update_in(i+2, :value) { |v| v + r[:value] }
        end

        # Replace
        number =
          number
            .delete_at(i)
            .delete_at(i)
            .insert(i, Immutable::Hash[depth: l[:depth] - 1, value: 0])

        break
      end
    end

    number
  end

  def split(number)
    number.each_index do |i|
      digit = number[i]
      if digit[:value] >= 10
        d = digit[:depth] + 1
        l = Immutable::Hash[depth: d, value: digit[:value] / 2]
        r = Immutable::Hash[depth: d, value: (digit[:value] / 2.0).ceil]

        return number.delete_at(i).insert(i, l, r)
      end
    end

    number
  end

  def unparse(number)
    i = 0

    # Needs to be declared because of circular reference.
    read_pair = nil

    read_value = Proc.new do |depth|
      digit = number[i]

      if digit[:depth] == depth
        i += 1
        digit[:value]
      else
        read_pair.call(depth + 1)
      end
    end

    read_pair = Proc.new do |depth|
      l = read_value.call(depth)
      r = read_value.call(depth)

      [l, r]
    end

    read_pair.call(1)
  end

  def parse(text)
    Immutable.from(text.each_line.collect { |line| parse_line(line.strip) })
  end

  def parse_line(line)
    depth = 0
    number = Immutable::Vector.empty

    line.each_char do |c|
      if c == '['
        depth += 1
      elsif c == ']'
        depth -= 1
      elsif c == ','
        # Ignore it
      else
        number = number << Immutable::Hash[depth: depth, value: c.to_i]
      end
    end

    number
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day18.run
end

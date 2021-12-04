# Advent of Code 2021
# Day 3: Binary Diagnostic

require 'immutable'

module Day03
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text = nil)
    text ||= input
    power_consumption(numbers(text))
  end

  def part_two(text = nil)
    text ||= input
    life_support_rating(numbers(text))
  end

  def life_support_rating(numbers)
    oxygen_rating(numbers) * co2_rating(numbers)
  end

  def oxygen_rating(numbers)
    binary = reduce_numbers_by(numbers, method(:most_common))
    binary.to_i(2)
  end

  def co2_rating(numbers)
    binary = reduce_numbers_by(numbers, method(:least_common))
    binary.to_i(2)
  end

  def reduce_numbers_by(numbers, proc)
    bit = 0
    while numbers.size > 1
      bits = aggregate_by_bit(numbers)
      value = proc.call(bits[bit])
      numbers = filter_by_bit(numbers, bit, value)
      bit += 1
    end

    numbers[0]
  end

  def filter_by_bit(numbers, bit, value)
    numbers.filter { |n| n[bit] == value }
  end

  def power_consumption(numbers)
    bits = aggregate_by_bit(numbers)
    gamma_rate(bits) * epsilon_rate(bits)
  end

  def gamma_rate(bits)
    binary = reduce_bits_by(bits, method(:most_common))
    binary.to_i(2)
  end

  def epsilon_rate(bits)
    binary = reduce_bits_by(bits, method(:least_common))
    binary.to_i(2)
  end

  def reduce_bits_by(bits, proc)
    chars = bits.map { |col| proc.call(col) }
    chars.join
  end

  def most_common(xs)
    groups = xs.group_by { |x| x }.values
    if all_same_length? groups then
      return '1'
    end

    groups.max_by(&:length)[0]
  end

  def least_common(xs)
    groups = xs.group_by { |x| x }.values
    if all_same_length? groups then
      return '0'
    end

    groups.min_by(&:length)[0]
  end

  def all_same_length?(xs)
    length = xs[0].length
    xs.all? { |x| x.length == length }
  end

  def aggregate_by_bit(numbers)
    # All of the numbers in the provided input are the same size,
    # so we can safely make that assumption here.
    bit_count = numbers[0].size
    empty = Immutable.from([[]]) * bit_count

    numbers.reduce(empty) do |bits, number|
      add_bits(number, bits)
    end
  end

  def add_bits(number, bits)
    number.chars.each_with_index do |c, i|
      bits = bits.update_in(i) { |v| v << c }
    end

    bits
  end

  def numbers(text)
    text.each_line.map(&:strip)
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day03.run
end

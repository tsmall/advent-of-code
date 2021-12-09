# Advent of Code 2021
# Day 8: Seven Segment Search

# TODO:
# This solution is slower than I'd like.
# I'm sure there's a faster solution,
# and I should try to come up with one sometime,
# but not tonight.

require 'immutable'

module Day08
  module_function

  NUMS = Immutable::Hash[
    0 => Immutable::Set['a', 'b', 'c', 'e', 'f', 'g'],
    1 => Immutable::Set['c', 'f'],
    2 => Immutable::Set['a', 'c', 'd', 'e', 'g'],
    3 => Immutable::Set['a', 'c', 'd', 'f', 'g'],
    4 => Immutable::Set['b', 'c', 'd', 'f'],
    5 => Immutable::Set['a', 'b', 'd', 'f', 'g'],
    6 => Immutable::Set['a', 'b', 'd', 'e', 'f', 'g'],
    7 => Immutable::Set['a', 'c', 'f'],
    8 => Immutable::Set['a', 'b', 'c', 'd', 'e', 'f', 'g'],
    9 => Immutable::Set['a', 'b', 'c', 'd', 'f', 'g'],
  ]

  DIGITS = NUMS.invert

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    entries = parse(text)

    entries
      .flat_map { |entry| entry[:outputs] }
      .count { |digit| easy?(digit) }
  end

  def part_two(text=nil)
    text ||= input
    entries = parse(text)

    total = 0
    entries.each do |entry|
      translation = find_translation(entry[:signals])
      total += decode_output(entry[:outputs], translation)
    end

    total
  end

  def easy?(digit)
    [2, 3, 4, 7].include?(digit.length)
  end

  def decode_output(outputs, translation)
    digits = outputs.collect { |digit| decode_digit(digit, translation) }
    digits.join.to_i
  end

  def decode_digit(digit, translation)
    unmapped = translate(digit, translation)
    DIGITS[unmapped]
  end

  def find_translation(signals)
    possible_translations.find { |t| match?(signals, t) }
  end

  def match?(signals, translation)
    signals = signals.collect { |digit| translate(digit, translation) }

    mapped_1 = signals.detect { |digit| digit.size == 2 }
    return false if mapped_1 != NUMS[1]
    signals = signals.delete(mapped_1)

    mapped_7 = signals.detect { |digit| digit.size == 3 }
    return false if mapped_7 != NUMS[7]
    signals = signals.delete(mapped_7)

    mapped_4 = signals.detect { |digit| digit.size == 4 }
    return false if mapped_4 != NUMS[4]
    signals = signals.delete(mapped_4)

    [NUMS[0], NUMS[2], NUMS[3], NUMS[5], NUMS[6], NUMS[8], NUMS[9]].each do |num|
      signals = signals.delete(num)
    end

    signals.empty?
  end

  def translate(digit, translation)
    digit.collect { |char| translation[char] }
  end

  def translation(dest)
    Immutable::Hash.new(dest.zip('a'..'g'))
  end

  def possible_translations
    possible_permutations.lazy.collect { |p| translation(p) }
  end

  def possible_permutations
    ('a'..'g').to_a.permutation(7)
  end

  def parse(text)
    text
      .each_line
      .to_list
      .collect { |line| parse_line(line) }
  end

  def parse_line(line)
    sections = line.split('|')
    signals, output = sections.collect(&:split)
    Immutable.from({
      signals: signals.collect { |digit| Immutable::Set.new(digit.chars) },
      outputs: output.collect { |digit| Immutable::Set.new(digit.chars) },
    })
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day08.run
end

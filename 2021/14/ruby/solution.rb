# Advent of Code 2021
# Day 14: Extended Polymerization

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end
end

module Day14
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    parse(text) => {pairs:, rules:, first_char:}
    result = polymerize(pairs, rules, steps: 10)
    diff(result, first_char)
  end

  def part_two(text=nil)
    text ||= input.read
    parse(text) => {pairs:, rules:, first_char:}
    result = polymerize(pairs, rules, steps: 40)
    diff(result, first_char)
  end

  def diff(pairs, first_char)
    tallies = tally(pairs, first_char)
    min, max = tallies.values.minmax
    max - min
  end

  def tally(pairs, first_char)
    result = Hash.new { 0 }
    result[first_char] = 1

    pairs.each do |pair, count|
      result[pair[1]] += count
    end

    Immutable.from(result)
  end

  def polymerize(pairs, rules, steps:)
    steps.times.inject(pairs) { |pairs, _| step(pairs, rules) }
  end

  def step(pairs, rules)
    pairs.inject(pairs) do |pairs, kv|
      pair, count = kv
      char = rules[pair]
      return pairs if not char

      new_a = pair[0] + char
      new_b = char + pair[1]
      pairs
        .put(pair)  { |n| n - count }
        .put(new_a) { |n| n + count }
        .put(new_b) { |n| n + count }
        .select { |pair, count| count > 0 }
    end
  end

  def parse(text)
    lines = text.lines

    template = lines.first.strip

    pairs = template.chars.each_cons(2).collect(&:join).tally
    pairs.default = 0

    rules = lines.drop(2).collect do |line|
      line.strip.split(' -> ')
    end

    Immutable::Hash[
      template: template,
      first_char: template[0],
      rules: Immutable::Hash.new(rules),
      pairs: Immutable::Hash.new(pairs) { 0 },
    ]
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day14.run
end

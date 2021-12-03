# Advent of Code 2021
# Day 2: Dive!

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end
end

module Day02
  module_function
  using Extensions

  START_POSITION = Immutable::Hash[depth: 0, position: 0, aim: 0]

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one
    run_with_interpretation(:move_part_one)
  end

  def part_two
    run_with_interpretation(:move_part_two)
  end

  def run_with_interpretation(name)
    follow(instructions, method(name)) => {depth:, position:}
    depth * position
  end

  def follow(instructions, move)
    instructions.inject(START_POSITION) do |position, instruction|
      move.call(position, instruction)
    end
  end

  def move_part_one(position, instruction)
    instruction => { command:, amount: }
    case command
    when :up
      position.put(:depth) { |n| n - amount }
    when :down
      position.put(:depth) { |n| n + amount }
    when :forward
      position.put(:position) { |n| n + amount }
    end
  end

  def move_part_two(position, instruction)
    instruction => { command:, amount: }
    case command
    when :up
      position.put(:aim) { |n| n - amount }
    when :down
      position.put(:aim) { |n| n + amount }
    when :forward
      position
        .put(:position) { |n| n + amount }
        .put(:depth) { |n| n + amount * position[:aim] }
    end
  end

  def instructions(input=nil)
    input ||= self.input
    input.each_line.collect { |line| parse(line) }
  end

  def parse(instruction)
    match = /(?<command>\w+) (?<amount>\d+)/.match(instruction)
    Immutable::Hash[
      command: match[:command].to_sym,
      amount: match[:amount].to_i,
    ]
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day02.run
end

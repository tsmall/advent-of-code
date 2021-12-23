# Advent of Code 2021
# Day 22: Reactor Reboot

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end
end

module Day22
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    steps = parse(text)
    on_cubes = reboot(steps)
    on_cubes.size
  end

  def part_two(text=nil)
    text ||= input
    0
  end

  def reboot(steps)
    steps.inject(Immutable::Set.empty, &method(:apply_step))
  end

  def apply_step(on_cubes, step)
    step => {xs:, ys:, zs:}

    out_of_range = [xs, ys, zs].any? { |r| r.first < -50 or r.last > 50 }
    return on_cubes if out_of_range

    changes = []
    xs.each do |x|
      ys.each do |y|
        zs.each do |z|
          bits = encode(x, y, z)
          changes << bits
        end
      end
    end

    case step[:action]
    when :on
      on_cubes + changes
    when :off
      on_cubes - changes
    end
  end

  def encode(x, y, z)
    bits = 0

    bits |= 0b10000000 if x < 0
    bits |= x.abs
    bits <<= 8

    bits |= 0b10000000 if y < 0
    bits |= y.abs
    bits <<= 8

    bits |= 0b10000000 if z < 0
    bits |= z.abs

    bits
  end

  def decode(bits)
    z = bits & 0b1111111
    z *= -1 if bits & 0b10000000 == 0b10000000

    bits >>= 8
    y = bits & 0b1111111
    y *= -1 if bits & 0b10000000 == 0b10000000

    bits >>= 8
    x = bits & 0b1111111
    x *= -1 if bits & 0b10000000 == 0b10000000

    [x, y, z]
  end

  STEP = /(?<action>on|off) x=(?<x1>-?\d+)..(?<x2>-?\d+),y=(?<y1>-?\d+)..(?<y2>-?\d+),z=(?<z1>-?\d+)..(?<z2>-?\d+)/

  def parse(text)
    text.each_line.collect do |line|
      match = STEP.match(line)
      Immutable::Hash[
        action: match[:action].to_sym,
        xs: match[:x1].to_i .. match[:x2].to_i,
        ys: match[:y1].to_i .. match[:y2].to_i,
        zs: match[:z1].to_i .. match[:z2].to_i,
      ]
    end
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day22.run
end

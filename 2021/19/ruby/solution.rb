# Advent of Code 2021
# Day 19: Beacon Scanner

require 'immutable'

module Day19
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    scanners = parse(text)
    map = build_map(scanners)
    map.size
  end

  def part_two(text=nil)
    text ||= input
    0
  end

  def build_map(scanners)
    all_beacons = scanners[0]
    rest = scanners.delete(0)

    while not rest.empty?
      puts "New iteration. [remaining=#{rest.size} all_beacons=#{all_beacons.size}]"
      rest.each do |id, scanner|
        puts "Trying to find a match. [id=#{id}]"
        result = find_overlaps(all_beacons, scanner)
        if result
          position, adjusted_beacons = result
          puts "Found a match! [id=#{id} position=#{position.inspect}]"

          all_beacons = all_beacons.union(adjusted_beacons)
          rest = rest.delete(id)
          break
        end
      end
    end

    all_beacons
  end

  def find_overlaps(a_beacons, b_beacons)
    a_beacons.each do |reference|
      each_orientation(b_beacons) do |b_oriented|
        b_oriented.each do |b|
          x_adjustment = reference[:x] - b[:x]
          y_adjustment = reference[:y] - b[:y]
          z_adjustment = reference[:z] - b[:z]

          b_adjusted = b_oriented.collect do |b|
            Immutable::Hash[
              x: b[:x] + x_adjustment,
              y: b[:y] + y_adjustment,
              z: b[:z] + z_adjustment,
            ]
          end

          if a_beacons.intersection(b_adjusted).size >= 12
            return Immutable::Vector[
              Immutable::Vector[x_adjustment, y_adjustment, z_adjustment],
              b_adjusted,
            ]
          end
        end
      end
    end

    # Nil indicates that no overlaps were found.
    nil
  end

  def each_orientation(beacons)
    orientations = [
      {x: :x, xd: +1, y: :y, yd: +1, z: :z, zd: +1},
      {x: :x, xd: +1, y: :z, yd: -1, z: :y, zd: +1},
      {x: :x, xd: +1, y: :y, yd: -1, z: :z, zd: -1},
      {x: :x, xd: +1, y: :z, yd: +1, z: :y, zd: -1},
      {x: :x, xd: -1, y: :y, yd: +1, z: :z, zd: -1},
      {x: :x, xd: -1, y: :z, yd: -1, z: :y, zd: -1},
      {x: :x, xd: -1, y: :y, yd: -1, z: :z, zd: +1},
      {x: :x, xd: -1, y: :z, yd: +1, z: :y, zd: +1},

      {x: :y, xd: +1, y: :x, yd: -1, z: :z, zd: +1},
      {x: :y, xd: +1, y: :z, yd: -1, z: :y, zd: -1},
      {x: :y, xd: +1, y: :x, yd: +1, z: :z, zd: -1},
      {x: :y, xd: +1, y: :z, yd: +1, z: :x, zd: +1},
      {x: :y, xd: -1, y: :x, yd: +1, z: :z, zd: +1},
      {x: :y, xd: -1, y: :z, yd: +1, z: :x, zd: -1},
      {x: :y, xd: -1, y: :x, yd: -1, z: :z, zd: -1},
      {x: :y, xd: -1, y: :z, yd: -1, z: :x, zd: +1},

      {x: :z, xd: +1, y: :y, yd: +1, z: :x, zd: -1},
      {x: :z, xd: +1, y: :x, yd: +1, z: :y, zd: +1},
      {x: :z, xd: +1, y: :y, yd: -1, z: :x, zd: +1},
      {x: :z, xd: +1, y: :x, yd: -1, z: :y, zd: -1},
      {x: :z, xd: -1, y: :y, yd: +1, z: :x, zd: +1},
      {x: :z, xd: -1, y: :x, yd: +1, z: :y, zd: -1},
      {x: :z, xd: -1, y: :y, yd: -1, z: :x, zd: -1},
      {x: :z, xd: -1, y: :x, yd: -1, z: :y, zd: +1},
    ]

    orientations.each do |o|
      oriented = beacons.collect do |b|
        Immutable::Hash[
          x: b[o[:x]] * o[:xd],
          y: b[o[:y]] * o[:yd],
          z: b[o[:z]] * o[:zd],
        ]
      end
      yield oriented
    end
  end

  def parse(text)
    xyz = [:x, :y, :z]
    scanners = {}

    sections = text.split("\n\n")
    sections.each do |section|
      lines = section.lines

      match = /--- scanner (\d+) ---/.match(lines[0])
      id = match[1].to_i

      beacons = lines[1..].collect do |line|
        nums = line.strip.split(',').collect(&:to_i)
        xyz.zip(nums).to_h
      end

      scanners[id] = beacons.to_set
    end

    Immutable.from(scanners)
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day19.run
end

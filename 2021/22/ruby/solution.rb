# Advent of Code 2021
# Day 22: Reactor Reboot

Cuboid = Struct.new(:value, :xs, :ys, :zs, keyword_init: true)

module Day22
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    cuboids = load_cuboids(text)
    count_on(cuboids, check_bounds: true)
  end

  def part_two(text=nil)
    text ||= input
    cuboids = load_cuboids(text)
    count_on(cuboids)
  end

  def count_on(cuboids, check_bounds: false)
    cubes_on = 0

    cuboids.each do |cuboid|
      if check_bounds
        next if cuboid.xs.first < -50 or cuboid.xs.last > 50
        next if cuboid.ys.first < -50 or cuboid.ys.last > 50
        next if cuboid.zs.first < -50 or cuboid.zs.last > 50
      end

      cubes_on += cuboid.value * cuboid.xs.size * cuboid.ys.size * cuboid.zs.size
    end

    cubes_on
  end

  def load_cuboids(text)
    cuboids = []

    parse(text).each do |new_cuboid|
      to_add = []
      cuboids.each do |existing_cuboid|
        intersection = intersect(existing_cuboid, new_cuboid)
        if intersection
          if intersection == existing_cuboid
            cuboids.delete(existing_cuboid)
          else
            intersection.value = existing_cuboid.value * -1
            to_add << intersection
          end
        end
      end

      cuboids.concat(to_add)
      cuboids << new_cuboid if new_cuboid.value == 1
    end

    cuboids
  end

  def intersect(a, b)
    return nil if a.xs.first > b.xs.last
    return nil if a.xs.last  < b.xs.first
    return nil if a.ys.first > b.ys.last
    return nil if a.ys.last  < b.ys.first
    return nil if a.zs.first > b.zs.last
    return nil if a.zs.last  < b.zs.first

    Cuboid.new(
      value: 0,
      xs: max(a.xs.first, b.xs.first) .. min(a.xs.last, b.xs.last),
      ys: max(a.ys.first, b.ys.first) .. min(a.ys.last, b.ys.last),
      zs: max(a.zs.first, b.zs.first) .. min(a.zs.last, b.zs.last),
    )
  end

  def min(x, y)
    x < y ? x : y
  end

  def max(x, y)
    x > y ? x : y
  end

  STEP = /(?<action>on|off) x=(?<x1>-?\d+)..(?<x2>-?\d+),y=(?<y1>-?\d+)..(?<y2>-?\d+),z=(?<z1>-?\d+)..(?<z2>-?\d+)/

  def parse(text)
    text.each_line.collect do |line|
      match = STEP.match(line)
      Cuboid.new(
        value: match[:action] == 'on' ? 1 : -1,
        xs: match[:x1].to_i .. match[:x2].to_i,
        ys: match[:y1].to_i .. match[:y2].to_i,
        zs: match[:z1].to_i .. match[:z2].to_i,
      )
    end
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day22.run
end

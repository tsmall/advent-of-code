# Advent of Code 2021
# Day 17: Trick Shot

Probe = Struct.new(:x, :y, :x_velocity, :y_velocity)
Target = Struct.new(:x_range, :y_range)

module Day17
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read.strip
    target = parse(text)
    best_arc(target)
  end

  def part_two(text=nil)
    text ||= input.read.strip
    target = parse(text)
    arcs = all_arcs(target)
    arcs.size
  end

  def best_arc(target)
    max_ys = []
    probe = Probe.new(0, 0, 0, 0)

    (1..target.x_range.max).each do |x_velocity|
      (1..1000).each do |y_velocity|
        probe.x = 0
        probe.y = 0
        probe.x_velocity = x_velocity
        probe.y_velocity = y_velocity

        result, max_y = fire(target, probe)
        if result == :hit
          max_ys << max_y
        end
      end
    end

    max_ys.max
  end

  def all_arcs(target)
    velocities = []
    probe = Probe.new(0, 0, 0, 0)

    (1..target.x_range.max).each do |x_velocity|
      (-1000..1000).each do |y_velocity|
        probe.x = 0
        probe.y = 0
        probe.x_velocity = x_velocity
        probe.y_velocity = y_velocity

        result, max_y = fire(target, probe)
        if result == :hit
          velocities << [x_velocity, y_velocity]
        end
      end
    end

    velocities
  end

  def fire(target, probe)
    max_y = 0

    while true
      step(probe)
      max_y = probe.y if probe.y > max_y

      return [:hit, max_y]    if hit?(target, probe)
      return [:missed, max_y] if missed?(target, probe)
    end
  end

  def hit?(target, probe)
    target.x_range.include?(probe.x) and target.y_range.include?(probe.y)
  end

  def missed?(target, probe)
    short_x = (probe.x_velocity.zero? and not target.x_range.include?(probe.x))
    past    = (probe.x > target.x_range.max or probe.y < target.y_range.min)

    short_x or past
  end

  def step(probe)
    probe.x += probe.x_velocity
    probe.y += probe.y_velocity

    probe.x_velocity -= 1 unless probe.x_velocity.zero?
    probe.y_velocity -= 1
  end

  def parse(text)
    match = /target area: x=(?<x_min>-?\d+)..(?<x_max>-?\d+), y=(?<y_min>-?\d+)..(?<y_max>-?\d+)/.match(text)
    Target.new(
      match[:x_min].to_i .. match[:x_max].to_i,
      match[:y_min].to_i .. match[:y_max].to_i,
    )
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day17.run
end

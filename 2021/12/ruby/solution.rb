# Advent of Code 2021
# Day 12: Passage Pathing

require 'immutable'

module Day12
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input
    map = parse(text)
    paths = find_all_paths(map)
    paths.size
  end

  def part_two(text=nil)
    text ||= input
    map = parse(text)
    paths = find_all_paths(map, allow_small_revisit: true)
    paths.size
  end

  def find_all_paths(map, allow_small_revisit: false)
    _find_all_paths(map, Immutable::Vector['start'], map['start'], allow_small_revisit)
  end

  def _find_all_paths(map, path_so_far, caves_from_here, allow_small_revisit)
    return Immutable::Vector.empty if not caves_from_here

    choices = caves_from_here.select do |cave|
      can_visit?(cave, path_so_far, allow_small_revisit)
    end

    choices.flat_map do |cave|
      if cave == 'end'
        Immutable::Vector[path_so_far.push(cave)]
      else
        _find_all_paths(map, path_so_far.push(cave), map[cave], allow_small_revisit)
      end
    end
  end

  def can_visit?(cave, path_so_far, allow_small_revisit)
    if not small?(cave)
      true
    else
      not path_so_far.include?(cave) or (
        allow_small_revisit and not have_revisited_a_small?(path_so_far)
      )
    end
  end

  def have_revisited_a_small?(path_so_far)
    smalls = path_so_far.select { |cave| small?(cave) }
    smalls != smalls.uniq
  end

  def small?(cave)
    /^[[:lower:]]+$/.match?(cave)
  end

  def parse(text)
    map = Immutable::Hash.new { Immutable::Vector.empty }

    text.each_line.inject(map) do |map, line|
      a, b = line.strip.split('-')
      map
        .put(a) { |paths| maybe_add(paths, b) }
        .put(b) { |paths| maybe_add(paths, a) }
    end
  end

  def maybe_add(paths, cave)
    if cave == 'start'
      paths
    else
      paths << cave
    end
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day12.run
end

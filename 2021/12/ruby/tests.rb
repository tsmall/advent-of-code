require 'immutable'
require './solution.rb'

module Day12::Tests
  module_function
  
  def test_all
    test_part_one
    test_part_two
  end

  def test_part_one
    puts "# Part One"

    expect_paths(
      map: Day12.parse("
        start-end
      "),
      expected: [['start', 'end']],
      description: 'Base case',
      part: :one,
    )

    expect_paths(
      map: Day12.parse("
        start-A
        A-end
      "),
      expected: [['start', 'A', 'end']],
      description: 'One path with three caves',
      part: :one,
    )

    expect_paths(
      map: Day12.parse("
        start-A
        start-B
        A-end
        B-end
      "),
      expected: [['start', 'A', 'end'], ['start', 'B', 'end']],
      description: 'One choice',
      part: :one,
    )

    expect_paths(
      map: Day12.parse("
        start-A
        start-b
        A-c
        A-b
        b-d
        A-end
        b-end
      "),
      expected: [
        ['start', 'A', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'b', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'end'],
        ['start', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'end'],
        ['start', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'b', 'A', 'end'],
        ['start', 'b', 'end'],
      ],
      description: 'First example from problem',
      part: :one,
    )

    expect_paths(
      map: Day12.parse("
        dc-end
        HN-start
        start-kj
        dc-start
        dc-HN
        LN-dc
        HN-end
        kj-sa
        kj-HN
        kj-dc
      "),
      expected: [
        ['start', 'HN', 'dc', 'HN', 'end'],
        ['start', 'HN', 'dc', 'HN', 'kj', 'HN', 'end'],
        ['start', 'HN', 'dc', 'end'],
        ['start', 'HN', 'dc', 'kj', 'HN', 'end'],
        ['start', 'HN', 'end'],
        ['start', 'HN', 'kj', 'HN', 'dc', 'HN', 'end'],
        ['start', 'HN', 'kj', 'HN', 'dc', 'end'],
        ['start', 'HN', 'kj', 'HN', 'end'],
        ['start', 'HN', 'kj', 'dc', 'HN', 'end'],
        ['start', 'HN', 'kj', 'dc', 'end'],
        ['start', 'dc', 'HN', 'end'],
        ['start', 'dc', 'HN', 'kj', 'HN', 'end'],
        ['start', 'dc', 'end'],
        ['start', 'dc', 'kj', 'HN', 'end'],
        ['start', 'kj', 'HN', 'dc', 'HN', 'end'],
        ['start', 'kj', 'HN', 'dc', 'end'],
        ['start', 'kj', 'HN', 'end'],
        ['start', 'kj', 'dc', 'HN', 'end'],
        ['start', 'kj', 'dc', 'end'],
      ],
      description: 'Second example from problem',
      part: :one,
    )
  end

  def test_part_two
    puts "# Part Two"

    expect_paths(
      map: Day12.parse("
        start-A
        start-b
        A-c
        A-b
        b-d
        A-end
        b-end
      "),
      expected: [
        ['start', 'A', 'b', 'A', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'b', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'b', 'A', 'b', 'end'],
        ['start', 'A', 'b', 'A', 'c', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'b', 'A', 'c', 'A', 'b', 'end'],
        ['start', 'A', 'b', 'A', 'c', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'b', 'd', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'b', 'd', 'b', 'A', 'end'],
        ['start', 'A', 'b', 'd', 'b', 'end'],
        ['start', 'A', 'b', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'A', 'b', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'd', 'b', 'A', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'd', 'b', 'end'],
        ['start', 'A', 'c', 'A', 'b', 'end'],
        ['start', 'A', 'c', 'A', 'c', 'A', 'b', 'A', 'end'],
        ['start', 'A', 'c', 'A', 'c', 'A', 'b', 'end'],
        ['start', 'A', 'c', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'c', 'A', 'end'],
        ['start', 'A', 'end'],
        ['start', 'b', 'A', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'b', 'A', 'b', 'A', 'end'],
        ['start', 'b', 'A', 'b', 'end'],
        ['start', 'b', 'A', 'c', 'A', 'b', 'A', 'end'],
        ['start', 'b', 'A', 'c', 'A', 'b', 'end'],
        ['start', 'b', 'A', 'c', 'A', 'c', 'A', 'end'],
        ['start', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'b', 'A', 'end'],
        ['start', 'b', 'd', 'b', 'A', 'c', 'A', 'end'],
        ['start', 'b', 'd', 'b', 'A', 'end'],
        ['start', 'b', 'd', 'b', 'end'],
        ['start', 'b', 'end'],
      ],
      description: 'First example from problem',
      part: :two,
    )
  end

  def expect_paths(map:, expected:, description:, part:)
    actual = Day12.find_all_paths(map, allow_small_revisit: part == :two)
    expected = Immutable.from(expected)

    if actual.sort == expected.sort
      puts "ok - #{description}"
    else
      puts "not ok - #{description}"
      puts "---"
      puts "expected:"
      pp expected.sort
      puts "actual:"
      pp actual.sort
      puts "..."
    end
  end
end

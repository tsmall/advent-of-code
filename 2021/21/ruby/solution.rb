# Advent of Code 2021
# Day 21: Dirac Dice

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end
end

module Day21
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    game = parse(text)
    end_game = play_game(game)
    losing_score(end_game) * end_game[:rolls]
  end

  def part_two(text=nil)
    text ||= input
    0
  end

  def init_game(p1_start, p2_start)
    Immutable::Hash[
      positions: Immutable::Vector[p1_start, p2_start],
      scores: Immutable::Vector[0, 0],
      die: 1,
      rolls: 0,
      player: 1,
    ]
  end

  def play_game(game)
    all_turns = Immutable.iterate(game) do |game|
      play_turn(game)
    end

    all_turns
      .drop_while { |game| not someone_won?(game) }
      .first
  end

  def someone_won?(game)
    game[:scores].any? { |s| s >= 1_000 }
  end

  def losing_score(game)
    game[:scores].min
  end

  def play_turn(game)
    game => {positions:, scores:, die:, rolls:, player:}

    i               = player - 1
    spaces, new_die = roll(game[:die])
    new_position    = move(positions[i], spaces)
    new_positions   = positions.set(i, new_position)
    new_scores      = scores.set(i) { |s| s + new_position }
    new_player      = player % 2 + 1

    game
      .put(:positions, new_positions)
      .put(:scores,    new_scores)
      .put(:die,       new_die)
      .put(:rolls,     rolls + 3)
      .put(:player,    new_player)
  end

  def move(position, spaces)
    (position + spaces - 1) % 10 + 1
  end

  def roll(die)
    rolls = 3.times.collect { |n| die_add(die, n) }
    [rolls.sum, die_add(rolls.last, 1)]
  end

  def die_add(die, n)
    (die + n - 1) % 100 + 1
  end

  def parse(text)
    p1_line, p2_line = text.lines
    p1_start = p1_line[28].to_i
    p2_start = p2_line[28].to_i
    init_game(p1_start, p2_start)
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day21.run
end

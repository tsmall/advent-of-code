# Advent of Code 2021
# Day 23: Amphipod

require 'immutable'

module Day23
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(state=nil)
    state ||= initial_input_state

    states = Immutable::Vector[state]
    found = nil

    until found
      states = tick(states)
      found = states.find(&method(:organized?))

      puts "Ticked. [states=#{states.size} found=#{not found.nil?}]"
    end

    found[:energy]
  end

  def part_two(text=nil)
    text ||= input
    0
  end

  def initial_example_state
    Immutable::Hash[
      energy: 0,
      positions: Immutable::Vector[
        Immutable::Vector[0, 0,  1,  0,  1,  0,  1,  0,  1,  0, 0],
        Immutable::Vector[0, 0, 'B', 0, 'C', 0, 'B', 0, 'D', 0, 0],
        Immutable::Vector[0, 0, 'A', 0, 'D', 0, 'C', 0, 'A', 0, 0],
      ],
    ]
  end

  def initial_input_state
    Immutable::Hash[
      energy: 0,
      positions: Immutable::Vector[
        Immutable::Vector[0, 0,  1,  0,  1,  0,  1,  0,  1,  0, 0],
        Immutable::Vector[0, 0, 'B', 0, 'B', 0, 'D', 0, 'A', 0, 0],
        Immutable::Vector[0, 0, 'D', 0, 'C', 0, 'A', 0, 'C', 0, 0],
      ],
    ]
  end

  def organized?(state)
    amphipod_positions(state).all? do |x, y|
      at_goal?(state, x, y)
    end
  end

  def amphipod_positions(state)
    mapped = (0..2).flat_map do |y|
      (0..10).collect do |x|
        [x, y] if state.dig(:positions, y, x).class == String
      end
    end

    mapped.compact
  end

  def tick(states)
    states.flat_map do |state|
      amphipod_positions(state).flat_map { |x, y| find_moves(state, x, y) }
    end
  end

  def find_moves(state, x, y)
    type = state.dig(:positions, y, x)
    cost = energy(type)

    states = Immutable::Vector.empty

    if y > 0
      # Already at its goal? No moves necessary.
      return states if at_goal?(state, x, y)

      # Can't move out of the room? No moves possible.
      return states if y == 2 and occupied?(state, x, 1)

      # Move the amphipod out of its original location for the states below.
      state = state.update_in(:positions, y, x) { 0 }

      orig_x = x
      steps = y
      y = 0

      (0...x).reverse_each do |x|
        break if occupied?(state, x, y)
        next  if forbidden?(state, x, y)
        states <<=
          state
            .put(:energy) { |e| e + (cost * (orig_x - x + steps)) }
            .update_in(:positions, y, x) { type }
      end

      (x+1..10).each do |x|
        break if occupied?(state, x, y)
        next  if forbidden?(state, x, y)
        states <<=
          state
            .put(:energy) { |e| e + (cost * (x - orig_x + steps)) }
            .update_in(:positions, y, x) { type }
      end
    elsif can_reach_goal?(state, x, y)
      x_goal = goal(type)
      y_goal = empty?(state, x_goal, 2) ? 2 : 1
      steps = (x_goal - x).abs + y_goal

      states <<=
        state
          .put(:energy) { |e| e + (cost * steps) }
          .update_in(:positions, y, x) { 0 }
          .update_in(:positions, y_goal, x_goal) { type }
    end

    states
  end

  def occupied?(state, x, y)
    state.dig(:positions, y, x).class == String
  end

  def forbidden?(state, x, y)
    state.dig(:positions, y, x) == 1
  end

  def empty?(state, x, y)
    state.dig(:positions, y, x) == 0
  end

  def at_goal?(state, x, y)
    type = state.dig(:positions, y, x)
    x_goal = goal(type)

    x == x_goal and (y == 2 or (y == 1 and at_goal?(state, x, 2)))
  end

  def can_reach_goal?(state, x, y)
    type = state.dig(:positions, y, x)
    x_goal = goal(type)

    path_clear = (x+1..x_goal).none? { |x| occupied?(state, x, y) }
    room_all_empty = empty?(state, x_goal, 1) and empty?(state, x_goal, 2)
    room_has_one_correct_type = empty?(state, x_goal, 1) and state.dig(:positions, 2, x_goal) == type
    room_ready = room_all_empty or room_has_one_correct_type
  end

  def goal(type)
    case type
    when 'A'
      2
    when 'B'
      4
    when 'C'
      6
    when 'D'
      8
    end
  end

  def energy(type)
    case type
    when 'A'
      1
    when 'B'
      10
    when 'C'
      100
    when 'D'
      1000
    end
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day23.run
end

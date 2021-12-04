# Advent of Code 2021
# Day 4: Giant Squid

require 'immutable'

module Extensions
  refine Immutable::Hash do
    def deconstruct_keys(keys)
      to_h
    end
  end
end

module Day04
  module_function
  using Extensions

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read
    parse_input(text) => {numbers:, boards:}
    play_to_first_winner(numbers, boards) => {
      number: winning_number,
      board: first_winning_board,
    }
    winning_number * sum_unmarked_numbers(first_winning_board)
  end

  def part_two(text=nil)
    text ||= input.read
    parse_input(text) => {numbers:, boards:}
    play_to_last_winner(numbers, boards) => {
      number: winning_number,
      board: last_winning_board,
    }
    winning_number * sum_unmarked_numbers(last_winning_board)
  end

  def play_to_first_winner(numbers, boards)
    numbers.each do |n|
      boards = mark_boards(boards, n)

      winning_board = check_for_winning_board(boards)
      if winning_board
        return Immutable::Hash[
          number: n,
          board: winning_board,
        ]
      end
    end

    raise 'Impossible! No winning board found.'
  end

  def play_to_last_winner(numbers, boards)
    numbers.each do |n|
      boards = mark_boards(boards, n)
      winning_boards = check_for_winning_boards(boards)

      if boards.size == 1 and boards[0] == winning_boards[0]
        return Immutable::Hash[
          number: n,
          board: boards[0],
        ]
      end

      boards = boards.reject { |board| winning_boards.include?(board) }
    end

    raise 'Impossible! No winning board found.'
  end

  def sum_unmarked_numbers(board)
    board => {rows:}

    rows_without_marked = rows.map do |row|
      row.select { |n| n != true }
    end

    rows_without_marked.flatten.sum
  end

  def mark_boards(boards, number)
    boards.map { |board| mark_board(board, number) }
  end

  def mark_board(board, number)
    board
      .put(:rows) { |rows| mark_collection(rows, number) }
      .put(:cols) { |cols| mark_collection(cols, number) }
  end

  def mark_collection(collection, number)
    collection.map do |numbers|
      numbers.map do |n|
        if n == number
          true
        else
          n
        end
      end
    end
  end

  def check_for_winning_board(boards)
    winning_boards = check_for_winning_boards(boards)

    case winning_boards.size
    when 0
      nil
    when 1
      winning_boards[0]
    else
      raise 'Expected a single winning board but found multiple.'
    end
  end

  def check_for_winning_boards(boards)
    boards.select { |board| winning_board?(board) }
  end

  def winning_board?(board)
    board => {rows:, cols:}
    winning_collection?(rows) or winning_collection?(cols)
  end

  def winning_collection?(collection)
    collection.any? do |numbers|
      numbers.all? { |each| each == true }
    end
  end

  def parse_input(text)
    sections = text.split("\n\n")
    numbers = sections[0].split(',').map(&:to_i)
    boards = sections[1..].map { |section| parse_board(section) }

    Immutable.from({
      numbers: numbers,
      boards: boards,
    })
  end

  def parse_board(text)
    board = Immutable::Hash[
      rows: Immutable::Vector[],
      cols: Immutable::Vector[],
    ]

    text.each_line do |line|
      numbers = line.split.map(&:to_i)
      board = board.put(:rows) { |rows| rows << Immutable.from(numbers) }

      numbers.each_with_index do |n, i|
        board = board.update_in(:cols, i) do |col|
          if col.nil?
            Immutable::Vector[n]
          else
            col << n
          end
        end
      end
    end

    board
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day04.run
end

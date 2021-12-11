# Advent of Code 2021
# Day 10: Syntax Scoring

require 'immutable'

module Day10
  module_function

  PAIRS = Immutable::Hash[
    '(' => ')',
    '[' => ']',
    '{' => '}',
    '<' => '>',
  ]

  CHECKER_SCORES = Immutable::Hash[
    ')' => 3,
    ']' => 57,
    '}' => 1197,
    '>' => 25137,
  ]

  AUTOCOMPLETE_SCORES = Immutable::Hash[
    ')' => 1,
    ']' => 2,
    '}' => 3,
    '>' => 4,
  ]

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input

    score = 0
    each_chunk(text) do |chunk|
      illegal = find_illegal_char(chunk)
      score += CHECKER_SCORES[illegal] if illegal
    end

    score
  end

  def part_two(text=nil)
    text ||= input

    valid_chunks = each_chunk(text).reject do |chunk|
      find_illegal_char(chunk)
    end

    scores = []
    valid_chunks.each do |chunk|
      missing_chars = finish(chunk)
      scores << autocomplete_score(missing_chars)
    end

    scores.sort!
    middle = scores.size / 2
    scores[middle]
  end

  def autocomplete_score(missing_chars)
    missing_chars.inject(0) do |score, char|
      score * 5 + AUTOCOMPLETE_SCORES[char]
    end
  end

  def finish(chunk)
    seen = []

    chunk.each_char do |char|
      if open_char? char
        seen.push(char)
      elsif close_char? char
        seen.pop()
      else
        raise "Unexpected character: #{char}"
      end
    end

    seen
      .collect { |open| PAIRS[open] }
      .reverse
  end

  def find_illegal_char(chunk)
    seen = []

    chunk.each_char do |char|
      if open_char? char
        seen.push(char)
      elsif close_char? char
        return char if char != PAIRS[seen.pop]
      else
        raise "Unexpected character: #{char}"
      end
    end

    nil
  end

  def open_char?(char)
    PAIRS.keys.include? char
  end

  def close_char?(char)
    PAIRS.values.include? char
  end

  def each_chunk(text)
    enum = text.each_line.lazy.collect { |line| line.strip() }

    if block_given?
      enum.each do |chunk|
        yield chunk
      end
    else
      enum
    end
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day10.run
end

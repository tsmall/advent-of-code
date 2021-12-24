# Advent of Code 2021
# Day 24: Arithmetic Logic Unit

module Day24
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(instructions=nil)
    instructions ||= input
    program = Program.new(instructions)

    each_model_number do |n|
      print "\rTrying a new serial number. [n=#{n}]"
      alu = ALU.new(n)
      program.run(alu)
      return n if alu.z == 0
    end

    nil
  end

  def each_model_number
    n = 99_999_999_999_999
    while n > 9_999_999_999_999
      yield n

      n -= 1
      while n.to_s.include? '0'
        n -= 1
      end
    end
  end

  def part_two(text=nil)
    text ||= input
    0
  end

  def input
    File.open("../input.txt")
  end
end

class Program
  attr_accessor :instructions

  def initialize(instructions)
    @instructions = instructions.each_line
  end

  def run(alu)
    instructions.rewind

    instructions.each do |line|
      command, *args = parse(line)
      alu.send(command, *args)
      # puts "Ran instruction. [command=#{command} args=#{args.inspect}]"
    end
  end

  def parse(line)
    command, *args = line.strip.split

    n = args.last.to_i
    args[-1] = n if n != 0

    [command, *args]
  end
end

class ALU
  attr_accessor :w, :x, :y, :z
  attr_accessor :inputs

  def initialize(input_number)
    @w = @x = @y = @z = 0
    @inputs = input_number.to_s.each_char.lazy.collect(&:to_i)
  end

  def inp(register)
    write(register, inputs.next)
  end

  def add(register_a, register_b)
    a = read(register_a)
    b = read(register_b)
    write(register_a, a + b)
  end

  def mul(register_a, register_or_value_b)
    a = read(register_a)
    b = read(register_or_value_b)
    write(register_a, a * b)
  end

  def div(register_a, register_b)
    a = read(register_a)
    b = read(register_b)
    write(register_a, a / b)
  end

  def mod(register_a, register_b)
    a = read(register_a)
    b = read(register_b)
    write(register_a, a % b)
  end

  def eql(register_a, register_b)
    a = read(register_a)
    b = read(register_b)
    write(register_a, a == b ? 1 : 0)
  end

  def read(register_or_value)
    if register?(register_or_value)
      send("#{register_or_value}")
    else
      register_or_value.to_i
    end
  end

  def write(register, n)
    check(register)
    send("#{register}=", n)
  end

  def check(register)
    if not register?(register)
      raise ArgumentError, "Invalid register (#{register})"
    end
  end

  def register?(register)
    instance_variables.include? "@#{register}".to_sym
  end
end

if $PROGRAM_NAME == __FILE__
  Day24.run
end

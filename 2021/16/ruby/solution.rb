# Advent of Code 2021
# Day 16: Packet Decoder

require 'immutable'

module Day16
  module_function

  def run
    puts "Part 1: #{part_one}"
    puts "Part 2: #{part_two}"
  end

  def part_one(text=nil)
    text ||= input.read.strip
    packet = parse(text)
    version_sum(packet)
  end

  def part_two(text=nil)
    text ||= input.read.strip
    packet = parse(text)
    evaluate(packet)
  end

  def version_sum(packet)
    packet[:version] + packet.fetch(:subpackets, []).inject(0) do |sum, packet|
      sum + version_sum(packet)
    end
  end

  def evaluate(packet)
    case packet[:type]
    when 0
      packet[:subpackets].inject(0) do |sum, packet|
        sum + evaluate(packet)
      end

    when 1
      packet[:subpackets].inject(1) do |product, packet|
        product * evaluate(packet)
      end

    when 2
      packet[:subpackets].collect { |packet| evaluate(packet) }.min

    when 3
      packet[:subpackets].collect { |packet| evaluate(packet) }.max

    when 4
      packet[:value]

    when 5
      sub1, sub2 = packet[:subpackets]
      evaluate(sub1) > evaluate(sub2) ? 1 : 0

    when 6
      sub1, sub2 = packet[:subpackets]
      evaluate(sub1) < evaluate(sub2) ? 1 : 0

    when 7
      sub1, sub2 = packet[:subpackets]
      evaluate(sub1) == evaluate(sub2) ? 1 : 0

    else
      raise "Invalid packet type for packet #{packet.inspect}"
    end
  end

  def parse(hex)
    size   = hex.size * 4
    binary = hex.to_i(16)

    bits_read = 0

    read = Proc.new do |n|
      mask = 0
      n.times { mask = (mask << 1) | 0b1 }

      offset = bits_read
      bits_read += n

      binary >> (size - n - offset) & mask
    end

    p = Proc.new do
      version = read.call(3)
      type    = read.call(3)

      packet = Immutable::Hash[
        version: version,
        type: type,
      ]

      if type == 4
        done = false
        value = 0
        until done
          block = read.call(5)
          value = (value << 4) | (block & 0b1111)
          done  = (block & 0b10000) == 0
        end

        packet.put(:value, value)
      else
        ltype = read.call(1)
        subpackets = Immutable::Vector.empty

        if ltype == 0
          length = read.call(15)
          offset = bits_read
          until bits_read - offset == length
            subpackets = subpackets << p.call()
          end
        else
          count = read.call(11)
          count.times do
            subpackets = subpackets << p.call()
          end
        end

        packet.put(:subpackets, subpackets)
      end
    end

    p.call
  end

  def input
    File.open("../input.txt")
  end
end

if $PROGRAM_NAME == __FILE__
  Day16.run
end

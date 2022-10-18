module Aoc2016
module Day16

input = "01000100010010111"

function part1()
    answer = generate(input, 272) |> checksum
    println("Part 1: $(join(answer))")
end

function part2()
    answer = generate(input, 35651584) |> checksum
    println("Part 2: $(join(answer))")
end

function generate(input, len)
    data = map(collect(input)) do c
        parse(Int, c)
    end

    while length(data) < len
        orig = view(data, :)
        push!(data, 0)
        for n in Iterators.reverse(orig)
            push!(data, n == 0 ? 1 : 0)
        end
    end

    view(data, 1:len)
end

function checksum(data)
    while length(data) |> iseven
        buffer = []
        for (n, m) in Iterators.partition(data, 2)
            push!(buffer, n == m ? 1 : 0)
        end
        data = buffer
    end

    data
end

end
end

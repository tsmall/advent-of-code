use v6;


# ------------------------------------------------------------------------------
# Graph Objects


class Edge {
    has Str $.source;
    has Str $.destination;
    has Int $.distance;

    method gist {
        return "$.source -> $.destination = $.distance";
    }

    method reversed {
        return Edge.new(
            source      => $.destination,
            destination => $.source,
            distance    => $.distance
        );
    }
}


class CityGraph {
    has Edge @.edges;
    has Int @!distances = ();

    method add(Edge $edge) {
        @.edges.push($edge);
    }

    method shortest-route-distance(--> Int) {
        return self!route-distances.min;
    }

    method longest-route-distance(--> Int) {
        return self!route-distances.max;
    }

    method !route-distances {
        if not @!distances {
            @!distances = @.edges.flatmap: -> $edge {
                self!routes-from(
                    current  => $edge,
                    rest     => @.edges.grep(!(* eqv $edge)),
                    distance => $edge.distance,
                    visited  => ($edge.source, $edge.destination),
                )
            };
        }
        return @!distances;
    }

    method !routes-from(:$current, :@rest, :$distance, :@visited) {
        my @options = self!options(@rest, $current.destination, @visited);

        if !@options {
            return [$distance];
        }

        return @options.flatmap: -> $next {
            self!routes-from(
                current  => $next,
                rest     => @rest.grep(!(* eqv $next)),
                distance => $distance + $next.distance,
                visited  => ($next.destination, |@visited),
            )
        }
    }

    method !options(@all-edges, Str $starting-from, @visited) {
        return @all-edges.grep: -> $option {
            ($starting-from eq $option.source) and ($option.destination ∉ @visited)
        }
    }
}


# ------------------------------------------------------------------------------
# Input Parsing


grammar DistanceParser {
    token TOP { <source> ' to ' <destination> ' = ' <distance> }

    token source      { \w+ }
    token destination { \w+ }
    token distance    { \d+ }
}


class DistanceParserActions {
    method TOP($/) {
        make Edge.new(
            source      => $<source>.made,
            destination => $<destination>.made,
            distance    => $<distance>.made
        );
    }

    method source($/) {
        make ~$/;
    }

    method destination($/) {
        make ~$/;
    }

    method distance($/) {
        make +$/;
    }
}


sub parse(Str $line) {
    state $actions = DistanceParserActions.new;
    my $result = DistanceParser.parse($line, actions => $actions);
    return $result.made;
}


# ------------------------------------------------------------------------------
# Main


my $cities = CityGraph.new;

for $*IN.lines -> $line {
    my $edge = parse($line);
    $cities.add($edge);
    $cities.add($edge.reversed);
}

say '--- Part One ---';
say 'Shortest Route: ', $cities.shortest-route-distance;

say '';

say '--- Part Two ---';
say 'Longest Route: ', $cities.longest-route-distance;

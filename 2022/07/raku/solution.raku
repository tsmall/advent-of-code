use v6;

my \small-dir-limit = 100_000;
my \total-disk-space = 70_000_000;
my \disk-space-required = 30_000_000;

sub MAIN() {
    my @path = ['/'];
    my %dir-sizes;

    for $*IN.lines -> $line {
        given $line {
            when / '$ cd ' (\w+) / {
                @path.push(~$0);
            }
            when '$ cd ..' {
                @path.pop;
            }
            when / (\d+) .* / {
                my $dir-path = '';
                for @path -> $dir {
                    $dir-path ~= $dir;
                    %dir-sizes{$dir-path} += $0;
                }
            }
        }
    }

    my $small-dir-size = 0;
    my $free-disk-space = total-disk-space - %dir-sizes</>;
    my $smallest-candidate-size = Nil;
    for %dir-sizes.values -> $size {
        if $size ≤ small-dir-limit {
            $small-dir-size += $size;
        }
        if ($free-disk-space + $size) ≥ disk-space-required {
            $smallest-candidate-size min= $size;
        }
    }

    say "Part 1: $small-dir-size";
    say "Part 2: $smallest-candidate-size";
}

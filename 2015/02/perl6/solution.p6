use v6;

grammar PresentDimensions {
    token TOP    { <length> 'x' <width> 'x' <height> }
    token length { \d+ }
    token width  { \d+ }
    token height { \d+ }
}

sub wrapping-paper-needed($present) {
    my @areas = (
        $present<length> * $present<width>,
        $present<width>  * $present<height>,
        $present<height> * $present<length>
    );

    my $surface-area = [+] @areas »*» 2;
    my $smallest-side = @areas.min;
    return $surface-area + $smallest-side;
}

sub ribbon-needed($present) {
    my @sides = $present<length>, $present<width>, $present<height>;
    my @two-shortest-sides = @sides.sort(&infix:«<=>»).head(2);
    my $ribbon-for-wrapping = 2 * @two-shortest-sides.sum;
    my $ribbon-for-bow = [*] @sides;
    return $ribbon-for-wrapping + $ribbon-for-bow;
}

my $total-wrapping-paper-needed = 0;
my $total-ribbon-needed = 0;

for $*IN.lines -> $line {
    my $present = PresentDimensions.parse($line);
    $total-wrapping-paper-needed += wrapping-paper-needed($present);
    $total-ribbon-needed += ribbon-needed($present);
}

say "The elves need to order:";
say "$total-wrapping-paper-needed sq ft of wrapping paper";
say "$total-ribbon-needed ft of ribbon";

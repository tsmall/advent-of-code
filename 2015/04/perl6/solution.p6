use v6;
use OpenSSL::Digest::MD5;


my $key = $*IN.get;

my $part-one-answer = Promise.new;
my $part-one-prefix = '00000';

my $part-two-answer = Promise.new;
my $part-two-prefix = '000000';

my $thread-count := 10;
(1..$thread-count).map: -> $thread-id {
    start {
        my $number = $thread-id;
        loop {
            last if $part-one-answer and $part-two-answer;

            my $digest = OpenSSL::Digest::MD5.new;
            $digest.add($key ~ $number);
            my $hash = $digest.hex;

            if not $part-one-answer and $hash.starts-with($part-one-prefix) {
                $part-one-answer.keep(($number.clone, $hash));
            }

            if not $part-two-answer and $hash.starts-with($part-two-prefix) {
                $part-two-answer.keep(($number.clone, $hash));
            }

            $number += $thread-count;
        }
    }
}

my ($part-one-number, $part-one-hash) = $part-one-answer.result;

say "";
say "--- Part One ---";
say "Key    = $key";
say "Hash   = $part-one-hash";
say "Number = $part-one-number";

my ($part-two-number, $part-two-hash) = $part-two-answer.result;

say "";
say "--- Part Two ---";
say "Key    = $key";
say "Hash   = $part-two-hash";
say "Number = $part-two-number";

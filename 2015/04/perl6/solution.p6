use v6;
use OpenSSL::Digest::MD5;

my $desired-prefix = '00000';

my $key = $*IN.get;

my $thread-count := 10;
my $answer = Promise.new;

(1..$thread-count).map: -> $thread-id {
    start {
        my $number = $thread-id;
        loop {
            last if $answer;

            my $digest = OpenSSL::Digest::MD5.new;
            $digest.add($key ~ $number);
            my $hash = $digest.hex;

            if $hash.starts-with($desired-prefix) {
                $answer.keep(($number, $hash));
                last;
            }

            $number += $thread-count;
        }
    }
}

my ($number, $hash) = $answer.result;

say "";
say "--- Part One ---";
say "Key    = $key";
say "Hash   = $hash";
say "Number = $number";

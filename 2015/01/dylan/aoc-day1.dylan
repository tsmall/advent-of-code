Module:    aoc-day1
Synopsis:  Solution to 2015 Advent of Code Day 1 problem
Author:    Tom Small


define method move (floor :: <integer>, direction :: <character>)
  let adjustment =
    select (direction)
      '(' => +1;
      ')' => -1;
      otherwise => 0;
    end;
  floor + adjustment;
end method move;


define method move-all (floor :: <integer>, directions :: <stream>, #key index = 1, basement-index = #f)
    => (floor :: <integer>, basement-index);
  if (stream-at-end?(directions))
    values(floor, basement-index);
  else
    let direction = read-element(directions);
    let new-floor = move(floor, direction);
    let new-basement-index = if (~basement-index & (new-floor < 0)) index else basement-index end;
    move-all(new-floor, directions, index: index + 1, basement-index: new-basement-index);
  end;
end method move-all;


define function main ()
  let (floor, basement-index) = move-all(0, *standard-input*);

  format-out("Part 1: %d\n", floor);
  format-out("Part 2: %=\n", basement-index);

  exit-application(0);
end function main;


main();

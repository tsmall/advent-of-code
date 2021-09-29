Module:   aoc-day2
Synopsis: Solution to 2015 Advent of Code Day 2 problem
Author:   Tom Small


define function main ()
  let total-paper = 0;
  let total-ribbon = 0;

  until (stream-at-end?(*standard-input*))
    let box = read-next-box(*standard-input*);
    total-paper := total-paper + paper-needed(box);
    total-ribbon := total-ribbon + ribbon-needed(box);
  end;

  format-out("Part 1: %d\n", total-paper);
  format-out("Part 2: %d\n", total-ribbon);

  exit-application(0);
end function main;


define constant <integer-triple> = limited(<vector>, of: <integer>, size: 3);


define class <box> (<object>)
  constant slot length :: <integer>, required-init-keyword: length:;
  constant slot width  :: <integer>, required-init-keyword: width:;
  constant slot height :: <integer>, required-init-keyword: height:;
end class;


define method print-object (box :: <box>, stream :: <stream>) => ()
  printing-object(box, stream)
    print(length(box), stream);
    print("x", stream, escape?: #f);
    print(width(box), stream);
    print("x", stream, escape?: #f);
    print(height(box), stream);
  end;
end method;


define method paper-needed (box :: <box>) => (paper-needed :: <integer>)
  surface-area(box) + apply(min, areas(box));
end method;


define method ribbon-needed (box :: <box>) => (ribbon-needed :: <integer>)
  volume(box) + apply(min, perimeters(box));
end method;


define method surface-area (box :: <box>) => (surface-area :: <integer>)
  2 * reduce(\+, 0, areas(box));
end method;


define method volume (box :: <box>) => (volume :: <integer>)
  length(box) * width(box) * height(box);
end method;


define method areas (box :: <box>) => (areas :: <integer-triple>)
  let results = make(<integer-triple>);

  results[0] := box.length * box.width;
  results[1] := box.width  * box.height;
  results[2] := box.height * box.length;

  results;
end method;


define method perimeters (box :: <box>) => (perimeters :: <integer-triple>)
  let results = make(<integer-triple>);

  results[0] := 2 * (box.length + box.width);
  results[1] := 2 * (box.width  + box.height);
  results[2] := 2 * (box.height + box.length);

  results;
end method;


define method read-next-box (input :: <stream>) => (box :: <box>)
  let length = string-to-integer(read-to(*standard-input*, 'x'));
  let width  = string-to-integer(read-to(*standard-input*, 'x'));
  let height = string-to-integer(read-to(*standard-input*, '\n'));

  make(<box>, length: length, width: width, height: height);
end method;


main();

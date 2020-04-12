import streams

proc main =
  var
    input = newFileStream(stdin)
    floor = 0
    index = 1
    enteredBasement = false
    c: char

  while not input.atEnd:
    c = input.readChar()
    if c == '(':
      inc floor
    elif c == ')':
      dec floor
    if floor < 0:
      enteredBasement = true
    if not enteredBasement:
      inc index

  echo "Part 1: ", floor
  echo "Part 2: ", index

main()

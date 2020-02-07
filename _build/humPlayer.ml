let turn (s:State.t) =
  print_endline ("Please enter a command. Current turn: " 
                 ^ (if s.turn = State.Black then "Black" else "White"));
  Stdlib.read_line()
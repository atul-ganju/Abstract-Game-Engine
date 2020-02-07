open OUnit2
open State
open Command

(* 
    Testing Methodology:

    The main two modules of our project to have test-worthy logic are state
    and command. The other modules are either very short (e.g. humPlayer) 
    essentially a few line input abstraction, visual printing (i.e. interface),
    or the main loop (i.e main) and thus hard to 'test' per OUnit. Thus, we 
    developed a suite of tests for each of these two files.

    For State, we wrote tests for each of the visible (i.e. in the mli) helper
    functions, such as kind_of_piece, using the dummy test_piece and test_game,
    as well as a fully initialized game of chess proper. For some of the
    functions, we *demonstrated* how it is possible to use them. For example,
    for we showed add_piece_of_json constructs some piece with the desired 
    properties from our written JSON, and that certain move commands were 
    either legal or not (e.g. from the start a pawn but not rook could move).
    We did these in a way to reach all cases of pattern matching etc. and it
    thus most closely is glass box testing.

    However, due to our project being not only game-based but more importantly
    data driven, many of the state functions would be incredibly unwieldy to 
    write even a single test case for, let along all possible outcomes. Take
    for example the saveing functions. These print out a string of 'JSON' text
    that supposedly represents the original rules of the game as well as the
    current piece locations. There would be no clear way to use OUnit to prove
    this correct. Also, commands like move change many aspects of the game
    object, and would be very hard to compare to (combined with the large number
    of exceptions and so on inherent in the rules of chess). Instead we made use 
    of extensive playtesting. By saving the game and loading from that, 
    continuing play, we were able to tell the function worked properly. Also,
    playing through games, trying to bring out corner cases, we could ensure 
    'move', 'take', 'castle' etc. and their associated helper functions worked
    (OUnit would do the same just much more verbosely and unruly).

    Conversely, as with the text adventure project, we were able to cover each
    possible parsing of an input string for our command module, and thus can
    be confident it works as expected. The helper functions not exposed were
    tested implicitly, and the error checking (e.g. out of bounds) had been 
    caught in playtesting.

    In our playing of the game and attempting to bring out corner cases and
    system failure, we have grown confident that the parts of the system works
    correctly and cohesively, and invite any chess players to give it a spin.
*)

let test_piece = {
  piece = King; color = White; loc = (4,5); first_moves = [];
  moves = []; attack_moves = []; first_move = true; 
  replace_zone = []; 
  replace_with = []; points = 50; en_passant = false; kinged = false} 

let test_game = {
  board = [test_piece]; board_size = 10; turn = White; 
  description = Yojson.Basic.from_file ("chess/new_game.json"); rules = []}

let test_chess = State.init_state 
    (Yojson.Basic.from_file ("chess/new_game.json"))

let state_tests = [
  "piece located (4,5) is location (4,5)" >:: 
  (fun _ ->  assert_equal
      (4,5) (piece_loc test_piece));
  "test piece is white" >:: (
    fun _ -> assert_equal
        White (piece_color test_piece));
  "game finds piece" >:: 
  (fun _ -> assert_equal
      (Some test_piece) (get_piece (4,5) test_game));
  "move king bad, pieces in way" >:: 
  (fun _ -> assert_equal
      (Illegal) 
      (move "king" (0,3) (0,4) test_chess));
  "move pawn ok, front clear" >:: 
  (fun _ -> assert_equal
      true 
      (match (move "pawn" (0,1) (0,2) test_chess) with
       |Legal _ -> true
       |Illegal -> false ));
  "move black pawn not ok, front clear but wrong turn" >:: 
  (fun _ -> assert_equal
      false 
      (match (move "pawn" (1,6) (1,5) test_chess) with
       |Legal _ -> true
       |Illegal -> false ));
  "board size 8 for chess" >:: 
  (fun _ -> assert_equal 8 (get_board_size test_chess));
  "piece option name extraction" >:: 
  (fun _ -> assert_equal King (kind_of_piece (Some test_piece)));
  "can construct piece from json" >:: 
  (fun _ -> assert_equal
      true
      (match (add_piece_of_json Pawn (4,6) White test_chess.description) with
       | {piece= Pawn; color = White; loc = (4,6); first_moves = _; 
          moves = _; attack_moves = _; first_move = true; 
          replace_zone = _; replace_with = _; points = _;} -> true
       | _ -> false));
  "'knight' -> Knight" >:: 
  (fun _ -> assert_equal Knight (string_to_piece "knight"));
  "test piece -> 'king'" >:: 
  (fun _ -> assert_equal "king" (piece_to_string test_piece));
  "fst snd three (1,2,3) -> (1,2)" >::
  (fun _ -> assert_equal (1,2) (fst_snd_three (1,2,3)));
  "trd three (1,2,3) -> (3)" >::
  (fun _ -> assert_equal (3) (trd_three (1,2,3)));
  "path between adjacent locs empty" >::
  (fun _ -> assert_equal [] (path (1,1) (2,2)));
  "path between 1 apart locs middle val, north" >::
  (fun _ -> assert_equal [(1,2)] (path (1,1) (1,3)));
  "path between 1 apart locs middle val, south" >::
  (fun _ -> assert_equal [(1,2)] (path (1,3) (1,1)));
  "path between 1 apart locs middle val, east" >::
  (fun _ -> assert_equal [(2,1)] (path (1,1) (3,1)));
  "path between 1 apart locs middle val, west" >::
  (fun _ -> assert_equal [(2,1)] (path (3,1) (1,1)));
  "path between 1 apart locs middle val, north-east" >::
  (fun _ -> assert_equal [(2,2)] (path (1,1) (3,3)));
  "path between 1 apart locs middle val, south-west" >::
  (fun _ -> assert_equal [(2,2)] (path (3,3) (1,1)));
  "path between 1 apart locs middle val, south-east" >::
  (fun _ -> assert_equal [(2,2)] (path (1,3) (3,1)));
  "path between 1 apart locs middle val, north-west" >::
  (fun _ -> assert_equal [(2,2)] (path (3,1) (1,3)));
  "path between 2 apart locs middle 2 val, north" >::
  (fun _ -> assert_equal [(1,2); (1,3)] (path (1,1) (1,4)));
  "path between 2 apart locs middle 2 val, south, ordered" >::
  (fun _ -> assert_equal [(1,3); (1,2)] (path (1,4) (1,1)));
  "path between non linear empty" >::
  (fun _ -> assert_equal [] (path (1,1) (3,2)));
  "one king, technically win condition" >::
  (fun _ -> assert_equal true (win_condition test_game));
  "zero kings, technically not win condition" >::
  (fun _ -> assert_equal false (win_condition {test_game with board = [];}));
]

let command_tests = [
  (* tests other funcs i.e. coordinate and parse and explode implicitly *)
  "quit command" >:: 
  (fun _ -> assert_equal Quit (parse "quit game" test_chess));
  "quit malformed" >::
  (fun _ -> assert_raises (Malformed "you did not enter a valid command") 
      (fun () -> parse "quit" test_chess));
  "save command" >::
  (fun _ -> assert_equal Save (parse "save game" test_chess));
  "save malformed" >::
  (fun _ -> assert_raises (Malformed "you did not enter a valid command") 
      (fun () -> parse "save" test_chess));
  "take command" >::
  (fun _ -> assert_equal (Take ("b", "a", (1,1), (0,0))) 
      (parse "take a on a1 with b on b2" test_chess));
  "replace command" >::
  (fun _ -> assert_equal (Replace "a") 
      (parse "replace pawn with a" test_chess));
  "replace malformed" >::
  (fun _ -> assert_raises (Malformed "you did not enter a valid command") 
      (fun () -> parse "replace " test_chess));
  "move command" >::
  (fun _ -> assert_equal (Move ("a", (0,0), (1,1))) 
      (parse "move a on a1 to b2" test_chess));
  "help command" >::
  (fun _ -> assert_equal Help (parse "help" test_chess));
  "help malformed" >::
  (fun _ -> assert_raises (Malformed "you did not enter a valid command") 
      (fun () -> parse "help something else" test_chess));
  "castle command" >::
  (fun _ -> assert_equal (Castle ("b", "a", (0,0), (1,1))) 
      (parse "castle b on a1 with a on b2" test_chess));
  "random stuff is malformed" >:: 
  (fun _ -> assert_raises (Malformed "you did not enter a valid command") 
      (fun () -> parse "wowof akew " test_chess));
  "empty" >:: 
  (fun _ -> assert_raises Empty 
      (fun () -> parse "" test_chess));
  "cancel" >::
  (fun _ -> assert_equal Cancel 
      (parse "cancel" test_chess));
  "en passant" >::
  (fun _ -> assert_equal (EnPassant ("a","b", (1,1), (2,2))) 
      (parse "en passant b on a7 with a on b2 moving it to c3" test_chess));
  "coordinate b2 <-> (1,1)" >::
  (fun _ -> assert_equal (1,1) (coordinate ['b';'2'] test_chess));
  "coordinate a1 <-> (0,0), edge case" >::
  (fun _ -> assert_equal (0,0) (coordinate ['a';'1'] test_chess));
  "coordinate h8 <-> (7,7), edge case" >::
  (fun _ -> assert_equal (7,7) (coordinate ['h';'8'] test_chess));
  "coordinate outside range" >::
  (fun _ -> assert_raises (Malformed "entered move outside of range") 
      (fun () -> coordinate ['h'; '9'] test_chess));
  "coordinate outside range for char ok, caught later" >::
  (fun _ -> assert_equal (8,7) (coordinate ['i';'8'] test_chess));
  "char outside range" >::
  (fun _ -> assert_raises (Malformed "not proper char") 
      (fun () -> coordinate ['!'; '9'] test_chess));
  "other stuff not valid" >::
  (fun _ -> assert_raises (Malformed "you did not enter a valid move") 
      (fun () -> coordinate ['5'; '9'; 'h'] test_chess));
  "explode 'e5' -> ['e';'5']" >::
  (fun _ -> assert_equal ['e';'5'] (explode "e5"));
]

let suite = 
  "test suite for chess" >::: List.flatten [
    state_tests;
    command_tests;
  ]

let _ = run_test_tt_main suite


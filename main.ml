open Command
open State

(** [get_load_game dir handle name] attempts to load a saved game of 
    name [name] located in [dir], recovering a game. *)
let rec get_load_game d od game_name = 
  try
    let file = Unix.readdir od in 
    if file = game_name^".json" then
      State.init_state (Yojson.Basic.from_file (d^Filename.dir_sep^file))
    else get_load_game d od game_name
  with e -> (Unix.closedir od); 
    print_endline ("\nThere was no saved game initialization " ^
                   "JSON in this directory.\n");
    raise End_of_file

(** [get_new_game dir handle] attempts to load a new game file located in [dir], 
    producing a game. *)
let rec get_new_game d od = 
  try
    let file = Unix.readdir od in 
    if file = "new_game.json" then
      try 
        State.init_state (Yojson.Basic.from_file (d^Filename.dir_sep^file))
      with exep -> print_endline "error initializing";
        get_new_game d od
    else get_new_game d od
  with e -> (Unix.closedir od); 
    print_endline ("\nThere was no new game initialization " ^  
                   "JSON in this directory.\n");
    raise End_of_file 

(** [save_game file_name game dir] creates a save file named [file_name] of 
    game [game] located in [dir]. *)
let save_game name state directory =
  let file = open_out (directory^Filename.dir_sep^name^".json") in
  (Printf.fprintf file "%s") (json_of_board state)

(** [check_directory dir] loads a game from within [dir] as prompted by user. 
    Recurses on new prompted directory name until correct game file can load. *)
let rec check_directory directory = 
  try (
    let game = Unix.opendir directory in
    print_endline 
      "\nPlease enter \"new game\" if you would like to play a new game.\n"; 
    print_endline 
      "Enter \"load [game name]\" if you would like to load a previously\n";
    print_endline 
      "saved game under then name you saved it: for example, \"load game1\"\n";
    print_string  "> ";
    let s = (read_line ()) in   
    match s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") with
    | "new"::"game"::[] -> (try (get_new_game directory game) with e ->   
        print_string "> ";    
        match read_line () with
        | directory -> check_directory directory)
    | "load"::game_name::[] -> 
      (try get_load_game directory game game_name with e ->
         print_endline 
           "\nInvalid saved game entered. Re-enter a directory name.\n";
         print_string "> ";
         match read_line () with
         | directory -> check_directory directory)
    | _ -> 
      print_endline "\nInvalid command entered. Re-enter a directory name.\n";
      print_string ("> ");
      match read_line () with
      | directory -> check_directory directory
  ) with e ->
    print_endline ("\nInvalid directory entered. Re-enter a directory name.\n");
    print_string ("> ");
    match read_line () with
    | file_name -> check_directory file_name

(** [other_color color] is the other color from [color]. *)
let other_color c =
  match c with
  | State.Black -> State.White
  | State.White -> State.Black

(** [replace_function game loc] replaces a piece in [game] at [loc],
    recursing until a valid replace is specified. *)
let rec replace_function state loc = 
  Interface.print_board state;
  print_endline "\n What would you like to replace your pawn with?\n";
  print_string "> ";
  match Command.parse (read_line ()) state with
  | Replace obj -> 
    (try {
       state with
       board = 
         (State.add_piece_of_json (State.string_to_piece obj)
            loc state.turn state.description)::
         (List.fold_left 
            (fun acc p -> if p.loc = loc then acc else p::acc) [] state.board);
       turn = other_color state.turn;
     }
     with e -> 
       print_endline ("\nNot a valid piece name. Try again.\n");
       replace_function state loc)
  | exception e -> 
    print_endline ("\nNot a valid command at this time. Try again.\n"); 
    replace_function state loc
  | _ -> 
    print_endline ("\nNot a valid command at this time. Try again.\n"); 
    replace_function state loc

(** [print_help] is the string detailing command information. *)
let print_help = 
  {|
---HELP---

Position Format: rowcol e.g. e2, d5, etc.

Commands:
- move "piece" on "start_position" to "end_position"
- quit game
- take "piece_1" on "position_1" with "piece_1" on "position_2"
- replace pawn with "piece"
- save game
- replace pawn with "piece"
- castle "piece_1" on "position_1" 
- en passant "piece_1" on "position_1" with "piece_2" on "position_2" 
moving it to "position_3"
|}

(** [move_function game obj1 loc1 loc2] attempts to execute moving 
    [obj1] from [loc1] to [loc2], either yielding the game with 
    updated locations and turn, or signalling the move is illegal. *)
let move_function state obj c1 c2 = 
  match (State.move obj c1 c2 state) with
  | State.Illegal ->
    Stdlib.print_endline "Illegal Move!";
    state
  | State.Legal(s) -> begin 
      match State.get_piece c2 s with 
      | Some piece -> 
        (if (List.filter (fun pos -> pos = c2 ) 
               piece.replace_zone <> []) then 
           if piece.piece <> Checker then replace_function s c2 else 
             {s with turn = (other_color s.turn)}
         else {s with turn = (other_color s.turn)})
      | _ -> {s with turn = other_color s.turn}
    end

(** [take_pos game] is whether a checker piece can take another piece
    after moving in [game]. *)
let take_pos state =
  let me_piece = List.find (fun p -> p.en_passant = true) state.board in
  (State.checker_options me_piece state) <> []

(** [take_function game obj1 obj2 loc1 loc2] attempts to execute taking 
    between [obj1] at [loc1] and [obj2] at [loc2], either yielding the game with 
    updated locations and turn, or signalling the move is illegal. *)
let rec take_function state obj1 obj2 c1 c2 = 
  match (State.take obj1 obj2 c1 c2 state) with
  | State.Illegal ->
    Stdlib.print_endline "Illegal Move!";
    state
  | State.Legal(s) -> begin 
      match State.get_piece c2 s with
      | Some piece -> 
        (if (List.filter (fun pos -> pos = c2 ) 
               piece.replace_zone <> []) then 
           replace_function s c2
         else {s with turn = (other_color s.turn)})
      | _ -> if List.mem "checker hop" state.rules then 
          begin print_endline "yay";
            if take_pos s then (
              print_endline "boo";
              Interface.print_board s;
              print_endline "You can take another checker. Do you choose to?";
              let turn = HumPlayer.turn s in begin 
                match (Command.parse turn state) with
                | Take (obj1, obj2, c1, c2) -> begin
                    try (take_function s obj1 obj2 c1 c2) 
                    with e -> take_function state obj1 obj2 c1 c2 end
                | Cancel -> {s with turn = (other_color s.turn)}
                | exception e -> print_endline "\nIllegal Command\n";
                  take_function state obj1 obj2 c1 c2
                | _ -> print_endline "\nIllegal Command\n";
                  take_function state obj1 obj2 c1 c2 end)
            else {s with turn = (other_color s.turn)} end
        else {s with turn = (other_color s.turn)}
    end

(** [castle_function game obj1 obj2 loc] attempts to execute castling between
    [obj1] and [obj2] at [loc], either yielding the game with updated locations 
    and turn, or signalling the move is illegal. *)
let castle_function state obj1 obj2 loc1 loc2 =
  match (State.castle obj1 obj2 loc1 loc2 state) with
  | State.Illegal ->
    Stdlib.print_endline "Illegal Castle!";
    state
  | State.Legal(s) ->
    {s with turn = (other_color s.turn)}

(** [passant_function state obj1 obj2 loc1 loc2] attempts to execute en 
    passant between [obj1] and [obj2] to [loc2], either yielding the 
    game with updated locations and turn, or signalling the move is illegal. *)
let passant_function state obj1 obj2 loc1 loc2 =
  match (State.passant obj1 obj2 loc1 loc2 state) with
  | State.Illegal ->
    Stdlib.print_endline "Illegal En Passant!";
    state
  | State.Legal(s) ->
    {s with turn = (other_color s.turn)}

(** [en_passant_off game] is [game] with none of the pieces elligible for
    en passant. *)
let en_passant_off state = 
  { 
    state with 
    board = 
      List.map (fun elt -> 
          if elt.color = state.turn && elt.piece = Pawn 
          then {elt with en_passant = false} else elt)
        state.board
  }

let rec play_the_rest state directory = 
  let state = en_passant_off state in
  if List.mem "check" state.rules then (
    begin 
      match check_checker state with
      | CheckMate -> print_endline "You are in checkmate #sadboyzzz\n"; 
        let winner = begin match state.turn with
          | Black -> "White wins!!!"
          | White -> "Black wins!!!" end in
        print_endline winner; exit 0
      | _ -> print_string "";
    end;)
  else if (List.mem "checker end" state.rules) then (
    if List.filter (fun p -> p.color = state.turn) state.board |> 
       List.length = 0 then
      let winner = begin 
        match state.turn with
        | Black -> "White wins!!!"
        | White -> "Black wins!!!" end in
      print_endline winner; exit 0)
  else print_string "";
  Interface.print_board state;
  if List.mem "check" state.rules then
    begin 
      match check_checker state with
      | Check -> print_endline "You are in Check!";
      | _ -> print_string "";
    end;
  let turn = HumPlayer.turn state in
  match (Command.parse turn state) with
  | Move(obj,c1, c2) -> begin
      try (play_the_rest (move_function state obj c1 c2) directory)
      with e -> (print_endline ("\nYou entered an invalid command\n"); 
                 play_the_rest state directory) end
  | Quit -> Stdlib.exit 0
  | Take (obj1, obj2, c1, c2) -> begin
      try (play_the_rest (take_function state obj1 obj2 c1 c2) directory)
      with e -> (print_endline ("\nYou entered an invalid command\n"); 
                 play_the_rest state directory) end
  | exception e -> Stdlib.print_endline "Illegal Command!";
    play_the_rest state directory
  | Save -> 
    Stdlib.print_endline "enter save file name: ";
    let file = read_line() in
    save_game file state directory
  | Help ->
    print_endline print_help;
    play_the_rest state directory
  | Castle(obj1,obj2,loc1,loc2) -> 
    if List.mem "check" state.rules &&
       List.mem "castle" state.rules then begin 
      try (play_the_rest (castle_function state obj1 obj2 loc1 loc2) directory)
      with e -> (print_endline ("\nCastling not possible\n"); 
                 play_the_rest state directory) end
    else (print_endline ("\nCastling not possible\n"); 
          play_the_rest state directory)
  | EnPassant(obj1,obj2,loc1,loc2) -> 
    if List.mem "en passant" state.rules then begin
      try (play_the_rest (passant_function state obj1 obj2 loc1 loc2) directory)
      with e -> (print_endline ("\nEn Passant not possible\n"); 
                 play_the_rest state directory) end
    else (print_endline ("\nEn Passant not possible\n"); 
          play_the_rest state directory)
  | _ -> play_the_rest state directory


let play_game directory = 
  let start = check_directory directory in 
  play_the_rest start directory

let main () =
  ANSITerminal.(print_string [red]
                  "\n\nWelcome to the our 3110 project.\n");
  print_endline "This is a virtual board game simulator.\n";
  print_endline "What type of game would you like to play?\n";
  print_endline 
    ("Please enter the name of the directory that r" ^ 
     "epresents the game you would like to play.\n");
  print_string  "> ";
  match read_line () with
  | exception End_of_file -> ()
  | directory -> play_game directory

(* Execute the game engine. *)
let () = main ()

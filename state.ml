open Yojson.Basic.Util

(** the exception corresponding to a position outside the board etc. *)
exception IllegalPosition

type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King | Checker

type check = NoCheck | Check | CheckMate

type color = Black | White

type location = (int*int)

type move = (int*int)

type obj = string

(* NOTE: moves given "e5" thus loc = col*row !!*)
type piece = {
  piece : piece_name;
  color : color;
  loc : location;
  first_moves : move list;
  moves : move list;
  attack_moves : move list;
  first_move : bool;
  replace_zone : location list;
  replace_with : obj list;
  points : int;
  en_passant : bool;
  kinged : bool;
}

type t = {
  board : piece list;
  board_size : int;
  turn : color;
  description : Yojson.Basic.t;
  rules : string list;
}

let get_piece pos game = 
  match List.find (fun piece -> piece.loc = pos) game.board with
  | exception (Not_found) -> None
  | a -> Some a

let get_board_size game = 
  game.board_size

type result = Legal of t | Illegal

(** [print_2 (a,b)] prints the string 'a, b ', and is useful for debugging 
    single positions generated in the game logic. *)
let print_2 elt = 
  match elt with
  | (a, b) -> print_string (string_of_int a ^ ", " ^ string_of_int b ^ "  ");;

(** [print (a,b,(c,d))] prints the string 'a, b, (c, d)', and is useful
    for debugging position pairs generated in the game logic, e.g. a piece's
    original position and to where it can move or take a piece. *)
let print elt = 
  match elt with
  | (a, b, c) -> print_endline (string_of_int a ^ ", " ^ string_of_int b ^ ", " 
                                ^ "(" ^ string_of_int (fst c) ^ ", " 
                                ^ string_of_int (snd c) ^ ")");;


let piece_loc p =
  p.loc

let piece_color p =
  p.color

(** [points_of_piece piece_name] is the point value associated with a 
    [piece_name] *)
let points_of_piece piece_name = 
  match piece_name with
  | Pawn -> 1
  | Knight -> 3
  | Bishop -> 3
  | Rook -> 5
  | Queen -> 9
  | King -> 10
  | Checker -> 0

(** [moves_of_json json] is the list of piece move locations 
    associated with the JSON representation [json]. 
    Requires: [json] is a valid representation for a piece. *)
let moves_of_json json = 
  ((json |> member "x" |> to_int), (json |> member "y" |> to_int))

(** [replace_zone_of_json json] is the list of replace zone locations 
    associated with the JSON representation [json]. 
    Requires: [json] is a valid representation for a piece. *)
let replace_zone_of_json json = 
  ((json |> member "x" |> to_int), (json |> member "y" |> to_int))

(** [replace_with_of_json json] is the list of 'replace with' piece types 
    associated with the JSON representation [json]. 
    Requires: [json] is a valid representation for a piece. *)
let replace_with_of_json json = 
  (json |> member "piece" |> to_string)

(** [rules_of_json json] is the list of rules of the JSON representation
    [json] Requires: [json] is a valid representation for a piece. *)
let rules_of_json json = 
  (json |> member "rule" |> to_string)

(** [piece_of_json json] is the piece specified by the JSON representation
    [json] Requires: [json] is a valid representation for a piece. *)
let piece_of_json json = {
  piece = begin
    match (json |> member "piece" |> to_string) with
    | "pawn" -> Pawn
    | "knight" -> Knight
    | "bishop" -> Bishop
    | "rook" -> Rook
    | "queen" -> Queen
    | "king" -> King
    | "checker" -> Checker
    | _ -> failwith "This is not a valid JSON. Edit it to the correct format."
  end;
  color = begin
    match json |> member "color" |> to_string with
    | "black" -> Black
    | "white" -> White
    | _ -> White (* bad *)
  end;
  loc = ((json |> member "col" |> to_int),(json |> member "row" |> to_int));
  first_moves = (try (
      json |> member "first_moves" |> to_list |> List.map moves_of_json
    )  with e -> []);
  moves = (json |> member "moves" |> to_list |> List.map moves_of_json);
  attack_moves = (try (
      json |> member "attack_moves" |> to_list |> List.map moves_of_json
    ) with e -> []);
  first_move = (try (json |> member "first_move" |> to_bool) with e -> true);
  replace_zone = (try (
      json |> member "replace_zone" |> to_list |> List.map replace_zone_of_json
    )  with e -> []);
  replace_with = (try (
      json |> member "replace_with" |> to_list |> List.map replace_with_of_json
    )  with e -> []);
  points = json |> member "points" |> to_int;
  en_passant = false;
  kinged = false;
}

(** [turn_of_json json] is the turn associated with the JSON
    representation [json]. 
    Requires: [json] is a valid representation for a game. *)
let turn_of_json j =
  match to_string j with
  | "white" -> White
  | "black" -> Black
  | _ -> failwith "This is not a valid JSON. Edit it to the correct format."

let init_state json = {
  board = json |> member "layout" |> to_list |> List.map piece_of_json;
  board_size = json |> member "size" |> to_int;
  turn = json |> member "turn" |> turn_of_json;
  description = json;
  rules = json |> member "rules" |> to_list |> List.map rules_of_json;
}

let piece_to_string p =
  match p.piece with
  | Pawn -> "pawn"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"
  | Checker -> "checker"

(** [piece_type_to_string p] is the string representation of [p]'s type. *)
let piece_type_to_string ptype =
  match ptype with
  | Pawn -> "pawn"
  | Knight -> "knight"
  | Bishop -> "bishop"
  | Rook -> "rook"
  | Queen -> "queen"
  | King -> "king"
  | Checker -> "checker"

let add_piece_of_json piece loc color json = {
  piece = piece;
  color = color;
  loc = loc;
  first_move = true;
  first_moves = (try (List.find (fun json -> 
      to_string (member "piece" json) = 
      (piece_type_to_string piece))
      (json |> member "layout" |> to_list)) 
                     |> member "first_moves" |> to_list 
                     |> List.map moves_of_json with e -> []);
  moves = (try (List.find (fun json -> 
      to_string (member "piece" json) = 
      (piece_type_to_string piece))
      (json |> member "layout" |> to_list)) 
               |> member "moves" |> to_list 
               |> List.map moves_of_json with e -> []);
  attack_moves = (try (List.find (fun json -> 
      to_string (member "piece" json) = 
      (piece_type_to_string piece))
      (json |> member "layout" |> to_list)) 
                      |> member "attack_moves" |> to_list 
                      |> List.map moves_of_json with e -> []);
  replace_zone = (try (List.find (fun json -> 
      to_string (member "piece" json) = 
      (piece_type_to_string piece))
      (json |> member "layout" |> to_list)) 
                      |> member "replace_zone" |> to_list 
                      |> List.map replace_zone_of_json with e -> []);
  replace_with = (try (List.find (fun json -> 
      to_string (member "piece" json) = 
      (piece_type_to_string piece))
      (json |> member "layout" |> to_list)) 
                      |> member "replace_with" |> to_list 
                      |> List.map replace_with_of_json with e -> []);
  points = (try (List.find (fun json -> to_string (member "piece" json) = 
                                        (piece_type_to_string piece))
                   (json |> member "layout" |> to_list)) 
                |> member "points" |> to_int  with e -> 0);
  en_passant = false;
  kinged = false;
}

let string_to_piece string = 
  match string with
  | "pawn" -> Pawn
  | "knight" -> Knight
  | "bishop" -> Bishop
  | "rook" -> Rook
  | "queen" -> Queen
  | "king" -> King
  | "checker" -> Checker
  | _ -> failwith "This is not a valid piece"

let piece_color_to_string p =
  match p.color with
  | Black -> "black"
  | White -> "white"

let json_of_piece p =
  let moves = (List.fold_left (fun acc mov -> 
      "{ \"x\": " ^ (string_of_int (fst mov)) ^ 
      {|, "y": |} ^ (string_of_int (snd mov)) ^ "}," ^ acc) "" p.moves) in
  let attack_moves = (List.fold_left (fun acc mov -> 
      "{ \"x\": " ^ (string_of_int (fst mov)) ^ 
      {|, "y": |} ^ (string_of_int (snd mov)) 
      ^ "}," ^ acc) "" p.attack_moves) in
  let first_moves = (List.fold_left (fun acc mov -> 
      "{ \"x\": " ^ (string_of_int (fst mov)) ^ 
      {|, "y": |} ^ (string_of_int (snd mov)) ^ "}," ^ acc) 
      "" p.first_moves) in
  let replace_zone = (List.fold_left (fun acc mov -> 
      "{ \"x\": " ^ (string_of_int (fst mov)) ^ 
      {|, "y": |} ^ (string_of_int (snd mov)) ^ "}," 
      ^ acc) "" p.replace_zone) in
  let replace_with = (List.fold_left (fun acc piece -> 
      "{ \"piece\": " ^ {|"|} ^ piece ^ {|"|} ^ "}," 
      ^ acc) "" p.replace_with) in
  {|
{
  "piece": |} ^ "\"" ^ piece_to_string p ^ "\"" ^ 
  {|, "points": |} ^ string_of_int p.points ^
  {|, "first_move": |} ^ string_of_bool p.first_move ^
  {|, "color": |} ^ "\"" ^ piece_color_to_string p ^ "\"" ^
  {|, "col": |} ^ string_of_int (fst (p.loc)) ^
  {|, "row": |} ^ string_of_int (snd (p.loc)) ^
  {|, "moves": [|} ^ (String.sub moves 0 (String.length moves - 1)) ^ {| ]|} ^
  (if attack_moves <> "" then
     {|, "attack_moves": [|} ^ (String.sub attack_moves 0 
                                  (String.length attack_moves - 1)) ^
     {| ]|} else "") ^ 
  (if first_moves <> "" then
     {|, "first_moves": [|} ^ (String.sub first_moves 0 
                                 (String.length first_moves - 1)) ^
     {| ]|} else "") ^ 
  (if replace_zone <> "" then  
     {|, "replace_zone": [|} ^  (String.sub replace_zone 0 
                                   (String.length replace_zone - 1)) ^ {| ]|}
   else "") ^
  (if replace_with <> "" then  
     {|, "replace_with": [|} ^  (String.sub replace_with 0 
                                   (String.length replace_with - 1)) ^ {| ]|}
   else "") ^
  {| },|}

(** [color_to_string color] is the string representation of [color]. *)
let color_to_string color =
  match color with
  | Black -> "black"
  | White -> "white"

(** [json_of_rule rule] is the json representation of [rule]. *)
let json_of_rule r =
  {|
{
  "rule": "|} ^ r ^ 
  {|"
  },|}

let json_of_board s =
  let json_pieces = List.fold_left (fun acc p -> json_of_piece p^acc) 
      "" s.board in
  let json_trimmed = String.sub json_pieces 0 
      (String.length json_pieces - 1) in
  let json_rules = List.fold_left (fun acc r -> json_of_rule r ^ acc) 
      "" s.rules in
  let rules_trimmed = String.sub json_rules 0 
      (String.length json_rules - 1) in
  {| { "layout": [ |} ^ json_trimmed ^ 
  {| ], "size": |} ^ string_of_int (s.board_size) ^ 
  {|, "turn": |} ^ {|"|} ^ color_to_string s.turn ^ 
  {|", "rules": [ |} ^ rules_trimmed ^ {|]}|}


(** [within_bounds loc game] is whether [loc] is within the bounds of 
    [game]'s board. *)
let within_bounds onto game =
  snd onto < game.board_size && snd onto >= 0 &&
  fst onto < game.board_size && fst onto >= 0

let kind_of_piece piece_option = 
  match piece_option with
  | None -> failwith "this cant happen"
  | Some piece -> piece.piece

(** [move_check_white moves from onto] is whether a white piece with [moves] 
    can move from [from] to [onto]. 
    Requires: JSON used to generate moves is valid. *)
let move_check_white moves from onto = 
  let delta_x = (fst onto) - (fst from) in
  let new_moves = List.filter (fun elt -> (fst elt) = delta_x) moves in 
  let delta_y = (snd onto) - (snd from) in
  let newer_moves = List.filter (fun elt -> (snd elt) = delta_y) new_moves in
  match newer_moves with
  | [] -> false
  | h::[] -> true
  | h::t -> print_endline "\n You did not give it a valid JSON file"; exit 0

(** [tuple_add t1 t2] is the tuple whose elements are the sum of [t1], [t2]. *)
let tuple_add t1 t2 = 
  match t1, t2 with
  | (a, b), (c, d) -> (a+c, b+d)

(** [checker_move_check_white moves from onto game] is the boolean that
    represents whether a checker's move inputted by the user is correct
    for a white piece given the game initialized*)
let checker_move_check_white moves from onto game = 
  let delta_x = if abs ((fst onto) - (fst from)) = (fst onto) - (fst from)
    then (fst onto) - (fst from) + 1
    else (fst onto) - (fst from) - 1 in
  let new_moves = List.filter (fun elt -> (fst elt) = delta_x) moves in 
  let delta_y = 
    if abs ((snd onto) - (snd from)) = (snd onto) - (snd from)
    then (snd onto) - (snd from) + 1
    else (snd onto) - (snd from) - 1 in
  let newer_moves = List.filter (fun elt -> (snd elt) = delta_y) new_moves in
  match newer_moves with
  | [] -> false
  | h::[] ->
    let new_loc = tuple_add h from in 
    List.length 
      (List.filter 
         (fun elt -> elt.loc = new_loc) game.board) = 0 &&
    fst new_loc >= -1 && snd new_loc > -1 &&
    fst new_loc < game.board_size && snd new_loc < game.board_size
  | h::t-> print_endline "\n You did not give it a valid JSON file"; exit 0

(** [move_check_black moves from onto] is whether a black piece with [moves] 
    can move from [from] to [onto]. 
    Requires: JSON used to generate moves is valid. *)
let move_check_black moves from onto = 
  let delta_x = (fst onto) - (fst from) in
  let new_moves = List.filter (fun elt -> - (fst elt) = delta_x) moves in 
  let delta_y = (snd onto) - (snd from) in
  let newer_moves = List.filter (fun elt -> - (snd elt) = delta_y) new_moves in
  match newer_moves with
  | [] -> false
  | h::[] -> true
  | h::t -> print_endline "\n You did not give it a valid JSON file"; exit 0

(** [checker_move_check_black moves from onto game] is the boolean that
    represents whether a checker's move inputted by the user is correct
    for a black piece given the game initialized*)
let checker_move_check_black moves from onto game = 
  let delta_x = if abs ((fst onto) - (fst from)) = (fst onto) - (fst from)
    then (fst onto) - (fst from) + 1
    else (fst onto) - (fst from) - 1 in
  let new_moves = List.filter (fun elt -> (fst elt) = - delta_x) moves in 
  let delta_y = 
    if abs ((snd onto) - (snd from)) = (snd onto) - (snd from)
    then (snd onto) - (snd from) + 1
    else (snd onto) - (snd from) - 1 in
  let newer_moves = List.filter (fun elt -> (snd elt) = - delta_y) new_moves in
  match newer_moves with
  | [] -> false
  | (a, b)::[] -> let new_loc = tuple_add (-a, -b) from in
    List.length 
      (List.filter 
         (fun elt -> elt.loc = new_loc) game.board) = 0 &&
    fst new_loc >= -1 && snd new_loc > -1 &&
    fst new_loc < game.board_size && snd new_loc < game.board_size
  | h::t-> print_endline "\n You did not give it a valid JSON file"; exit 0

let path coord1 coord2 = 
  if (fst coord1) = (fst coord2) then 
    List.init (abs((snd coord1) - (snd coord2))-1) 
      (fun i -> ((fst coord1), (if (snd coord1)>(snd coord2) 
                                then ((snd coord1)-i-1) 
                                else ((snd coord1)+i+1))))
  else if (snd coord1) = (snd coord2) then 
    List.init (abs((fst coord1) - (fst coord2))-1) 
      (fun i -> ((if (fst coord1)>(fst coord2) 
                  then ((fst coord1)-i-1) 
                  else ((fst coord1)+i+1)), (snd coord1)))
  else if abs ((fst coord1) - (fst coord2)) 
          = abs ((snd coord1) - (snd coord2)) then 
    List.init (abs((fst coord1) - (fst coord2))-1) 
      (fun i -> ((if (fst coord1)>(fst coord2) 
                  then ((fst coord1)-i-1) else ((fst coord1)+i+1)), 
                 (if (snd coord1)>(snd coord2) 
                  then ((snd coord1)-i-1) else ((snd coord1)+i+1))))
  else []

let is_valid_move obj from onto game = 
  (* checking color of selected piece matches game turn *)
  let from_piece = (get_piece from game) in
  if 
    match from_piece with
    | Some a -> a.color <> game.turn
    | None -> true
  then false
  else
  if (kind_of_piece (from_piece) = string_to_piece obj && 
      (get_piece onto game) = None && 
      (List.for_all (fun loc -> (get_piece loc game) = None) (path from onto))) 
  then 
    match from_piece with
    | None -> false (* shan't happen *) 
    | Some piece -> begin
        match piece.color, piece.first_move with
        | White, true ->
          if (piece.first_moves <> []) then
            move_check_white piece.first_moves from onto
          else move_check_white piece.moves from onto
        | White, false ->
          move_check_white piece.moves from onto
        | Black, true ->
          if (piece.first_moves <> []) then 
            move_check_black piece.first_moves from onto
          else move_check_black piece.moves from onto
        | Black, false ->
          move_check_black piece.moves from onto
      end
  else false

let is_valid_take obj1 obj2 from onto game = 
  let from_piece = (get_piece from game) in
  let onto_piece = (get_piece onto game) in 
  if match from_piece, onto_piece with
    | Some a, Some b -> a.color <> game.turn || b.color = game.turn
    | _ -> false
  then false
  else
  if (kind_of_piece from_piece = string_to_piece obj1 &&
      kind_of_piece onto_piece = string_to_piece obj2 &&
      (get_piece onto game) <> None) && 
     (List.for_all (fun loc -> (get_piece loc game) = None) 
        (path from onto)) then 
    match from_piece with
    | None -> false (*Never going to happen *) 
    | Some piece -> begin
        match piece.color, piece.piece with
        | White, Checker -> 
          checker_move_check_white piece.attack_moves from onto game
        | White, _ -> if (piece.attack_moves = []) then 
            (move_check_white piece.moves from onto)
          else move_check_white piece.attack_moves from onto
        | Black, Checker -> 
          checker_move_check_black piece.attack_moves from onto game
        | Black, _ -> if (piece.attack_moves = []) then 
            (move_check_black piece.moves from onto)
          else move_check_black piece.attack_moves from onto
      end
  else false

(** [is_valid_checker_take obj1 obj2 from onto game] is whether the specified
    take is valid in checkers. *)
let is_valid_checker_take obj1 obj2 from onto game = 
  let from_piece = (get_piece from game) in
  let onto_piece = (get_piece onto game) in 
  if match from_piece, onto_piece with
    | Some a, Some b -> a.color <> game.turn || b.color = game.turn
    | _ -> false
  then false
  else
  if (kind_of_piece from_piece = string_to_piece obj1 &&
      (get_piece onto game) <> None) && 
     (List.for_all (fun loc -> (get_piece loc game) = None) 
        (path from onto)) then 
    match from_piece with
    | None -> false (*Never going to happen *) 
    | Some piece -> begin
        match piece.color, piece.piece with
        | White, Checker -> 
          checker_move_check_white piece.attack_moves from onto game
        | White, _ -> if (piece.attack_moves = []) then 
            (move_check_white piece.moves from onto)
          else move_check_white piece.attack_moves from onto
        | Black, Checker -> 
          checker_move_check_black piece.attack_moves from onto game
        | Black, _ -> if (piece.attack_moves = []) then 
            (move_check_black piece.moves from onto)
          else move_check_black piece.attack_moves from onto
      end
  else false

(** [is_valid_take obj loc game] is whether a move with
    the given object and location would be valid in [game], without
    checking of piece type present. *)
let is_valid_take_no_type from onto game =
  let from_color = match get_piece from game with 
    | None -> failwith "this cant happen" (** this cant happen*)
    | Some a -> a.color in
  try let onto_color = match get_piece onto game with 
      | None -> failwith "this cant happen" (** this cant happen*)
      | Some a -> a.color in 
    let in_between = (path from onto) in 
    let path_bool = if in_between = [] then true 
      else (List.for_all 
              (fun loc -> (get_piece loc game) = None) (in_between)) in
    if (get_piece onto game <> None) then 
      (if from_color <> onto_color then
         path_bool
       else false) else false
  with e -> false

(** [is_valid_move_no_type from onto game] is [is_valid_move] witout 
    requirement of piece type, for use in internal logic 
    rather than player specification. *)
let is_valid_move_no_type from onto game =
  let from_piece = (get_piece from game) in
  try let onto_piece = (get_piece onto game) in 
    if match from_piece, onto_piece with
      | Some a, Some b -> a.color <> game.turn || b.color = game.turn
      | _ -> false
    then false
    else
    if (get_piece onto game) <> None && 
       (List.for_all 
          (fun loc -> (get_piece loc game) = None) (path from onto)) then 
      match from_piece with
      | None -> false (*Never going to happen *) 
      | Some piece -> begin
          match piece.color with
          | White -> if (piece.attack_moves = []) then 
              (move_check_white piece.moves from onto)
            else move_check_white piece.attack_moves from onto
          | Black -> if (piece.attack_moves = []) then 
              (move_check_black piece.moves from onto)
            else move_check_black piece.attack_moves from onto
        end
    else false
  with e -> false

(** [fst_three (a,b,c)] is the first element of a 3-tuple. *)
let fst_three = function
  | (a, b, c) -> a

(** [snd_three (a,b,c)] is the second element of a 3-tuple. *)
let snd_three = function
  | (a, b, c) -> b

let piece_move_options piece game =
  let moves = List.fold_left 
      (fun acc mov -> if is_valid_move (piece_to_string(piece)) piece.loc 
          ((if piece.color = White then fst mov 
            else - fst mov) + (fst piece.loc), 
           (if piece.color = White then snd mov 
            else - snd mov) + (snd piece.loc)) game 
        then ((if piece.color = White then fst mov 
               else - fst mov) + (fst piece.loc), 
              (if piece.color = White then snd mov 
               else - snd mov) + (snd piece.loc), piece.loc)::acc else acc) [] 
      (if piece.first_move then piece.first_moves@piece.moves 
       else piece.moves) in 
  (** MAKE SURE THEY ARE VALID MOVES*)
  let valid = List.filter 
      (fun elt -> fst_three elt >= 0 && fst_three elt < get_board_size game 
                  && snd_three elt >= 0 && snd_three elt < get_board_size game) 
      moves in 
  (*List.iter (fun elt -> print elt) valid;*)
  valid

(** [piece_take_options p game] is the list of valid takes of [p] in [game],
    containing the absolute final and initial locations. It intersperses
    strings containing the list of takes for each piece, for debugging 
    along with [path]. *)
let piece_take_options piece game =
  (*print_endline (piece_to_string piece);*)
  let atck_moves = List.fold_left (fun acc mov ->
      if is_valid_take_no_type piece.loc 
          ((if piece.color = White then fst mov 
            else - fst mov) + (fst piece.loc), 
           (if piece.color = White then snd mov 
            else - snd mov) + (snd piece.loc)) game 
      then ((if piece.color = White then fst mov 
             else - fst mov) + (fst piece.loc), 
            (if piece.color = White then snd mov 
             else - snd mov) + (snd piece.loc), piece.loc)::acc else acc) [] 
      (if piece.attack_moves = [] then piece.moves else piece.attack_moves) in
  (** MAKE SURE THEY ARE VALID MOVES*)
  let valid = List.filter 
      (fun elt -> fst_three elt >= 0 && fst_three elt < get_board_size game 
                  && snd_three elt >= 0 && snd_three elt < get_board_size game) 
      atck_moves in 
  valid


let piece_take_options_2 piece game =
  let atck_moves = List.fold_left (fun acc mov ->
      if is_valid_take_no_type piece.loc 
          ((if piece.color = White then fst mov 
            else - fst mov) + (fst piece.loc), 
           (if piece.color = White then snd mov 
            else - snd mov) + (snd piece.loc)) game 
      then ((if piece.color = White then fst mov 
             else - fst mov) + (fst piece.loc), 
            (if piece.color = White then snd mov 
             else - snd mov) + (snd piece.loc))::acc else acc) [] 
      (if piece.attack_moves = [] then piece.moves else piece.attack_moves) in
  (** MAKE SURE THEY ARE VALID MOVES*)
  let valid = List.filter 
      (fun elt -> fst elt >= 0 && fst elt < get_board_size game 
                  && snd elt >= 0 && snd elt < get_board_size game) 
      atck_moves in 
  valid

let checker_options piece game =
  let atck_moves = List.fold_left (fun acc mov ->
      if is_valid_checker_take "checker" "checker" piece.loc 
          ((if piece.color = White then fst mov 
            else - fst mov) + (fst piece.loc), 
           (if piece.color = White then snd mov 
            else - snd mov) + (snd piece.loc)) game 
      then ((if piece.color = White then fst mov 
             else - fst mov) + (fst piece.loc), 
            (if piece.color = White then snd mov 
             else - snd mov) + (snd piece.loc))::acc else acc) [] 
      piece.moves in
  (** MAKE SURE THEY ARE VALID MOVES*)
  let valid = List.filter 
      (fun elt -> fst elt >= 0 && fst elt < get_board_size game 
                  && snd elt >= 0 && snd elt < get_board_size game) 
      atck_moves in 
  valid


let fst_snd_three elt = 
  match elt with 
  | (a, b, c) -> (a, b)

let trd_three elt = 
  match elt with 
  | (a, b, c) -> c

(** [piece_options piece game] is the list of move and take options of 
    [piece] in [game]. *)
let piece_options piece game = 
  List.fold_left (fun acc elt-> (fst_snd_three elt) :: acc) [] 
    ((piece_take_options piece game) @ (piece_move_options piece game))

(** [path_blockable checked_moves game check_moves] is whether [checked_moves] 
    - the players moves - can block any of [check_moves] - the moves
      putting the king in check. *)
let rec path_blockable checked_moves game = function
  | [] -> false
  | h::[] -> List.mem h checked_moves
  | h::t -> if List.mem h checked_moves then true 
    else path_blockable checked_moves game t

(** [check_helper game lst] is the moves the king can get out
    of check from, or whether it is in check(mate). *)
let rec check_helper game lst = 
  let rec get_all_takes board = 
    match board with
    | [] -> []
    | h::t -> (piece_take_options h game)@(get_all_takes t) in
  let opponent_moves = (get_all_takes (List.filter (fun elt -> 
      elt.color <> game.turn) game.board)) in
  match lst with
  | [] -> CheckMate
  | h::t -> 
    let check_moves = List.filter 
        (fun elt -> h = fst_snd_three elt) opponent_moves in
    if check_moves <> [] then Check else check_helper game t

let check_checker game = 
  let king_location = 
    match (List.filter 
             (fun piece -> piece.piece = King 
                           && piece.color = game.turn) game.board) with
    | [] -> failwith "this cant happen"
    | h::[] -> h.loc
    | h::t -> failwith "this cant happen" in
  let rec get_all_takes board = 
    match board with
    | [] -> []
    | h::t -> (piece_take_options h game)@(get_all_takes t) in
  let rec get_all_takes_2 board = 
    match board with
    | [] -> []
    | h::t -> (piece_take_options_2 h game)@(get_all_takes_2 t) in
  let rec get_all_moves board = 
    match board with
    | [] -> []
    | h::t -> (piece_move_options h game)@(get_all_moves t) in
  let opponent_moves = get_all_takes 
      (List.filter (fun elt -> elt.color <> game.turn) game.board) in
  let player_moves = get_all_moves 
      (List.filter (fun elt -> elt.color = game.turn && elt.piece <> King) 
         game.board) in
  let player_takes = get_all_takes_2 
      (List.filter (fun elt -> elt.color = game.turn && elt.piece <> King) 
         game.board) in
  let checked_moves = List.fold_left 
      (fun acc elt -> (fst_snd_three elt)::acc) [] player_moves in 
  let check_moves = List.filter 
      (fun elt -> king_location = fst_snd_three elt) opponent_moves in
  let movable = check_helper game 
      (piece_options 
         (List.hd 
            (List.filter 
               (fun piece -> piece.piece = King && piece.color = game.turn) 
               game.board)) game) in 
  if List.length check_moves <> 0 then 
    if List.length check_moves = 1 then 
      begin 
        let takable = List.mem (trd_three (List.hd check_moves)) 
            player_takes in 
        let blockable =
          path_blockable checked_moves game 
            (path king_location (trd_three (List.hd check_moves))) in 
        if blockable || takable then Check else movable end 
    else movable
  else NoCheck

let win_condition game =
  (List.filter (fun p -> p.piece = King) game.board) |> List.length = 1 

let move obj from onto game =
  if is_valid_move obj from onto game then
    let potential = 
      {
        game with
        board = 
          (game.board) 
          |> List.map 
            (fun p -> if p.loc = from then
                {p with 
                 loc = onto; 
                 first_move = false; 
                 en_passant = 
                   p.piece = Pawn 
                   && abs (snd from - snd onto) = 2;
                 kinged = if p.piece = Checker then 
                     if List.mem (onto) p.replace_zone then
                       true else false else false;
                 attack_moves = 
                   if List.mem onto p.replace_zone && p.kinged = false 
                   then (-2, -2)::(2,-2)::p.attack_moves else p.attack_moves;
                 moves =  if List.mem onto p.replace_zone && p.kinged = false 
                   then (-1, -1)::(1,-1)::p.moves else p.moves;} else p)

      } in 
    if List.mem "check" game.rules then 
      if (check_checker potential <> NoCheck) 
      then Illegal 
      else Legal (potential) 
    else Legal potential
  else Illegal

(** [tuple_neg (a,b)] is (-a,-b) *)
let tuple_neg t = 
  match t with
  | (a, b) -> (-a, -b)

let take obj1 obj2 from onto game = 
  if is_valid_take obj1 obj2 from onto game then 
    let potential = 
      {
        game with
        board = List.fold_left (fun acc p -> 
            if p.loc = from
            then
              if p.piece = Checker then 
                let new_loc = 
                  tuple_add (tuple_add onto (tuple_neg from)) onto in 
                {p with 
                 loc = new_loc;
                 first_move = false;
                 kinged = 
                   if p.piece = Checker && p.kinged = false then 
                     if List.mem new_loc p.replace_zone then
                       true else false 
                   else p.piece = Checker && p.kinged = true ;
                 attack_moves = 
                   if 
                     List.mem new_loc
                       p.replace_zone && p.kinged = false 
                   then (-2, -2)::(2,-2)::p.attack_moves else p.attack_moves;
                 moves = if List.mem new_loc p.replace_zone && p.kinged = false 
                   then (-1, -1)::(1,-1)::p.moves else p.moves;
                 en_passant = true;
                }::acc 
              else {p with loc = onto; first_move = false}::acc 
            else if p.loc = onto then acc
            else p::acc) [] game.board
      } in
    if List.mem "check" game.rules then 
      if (check_checker potential <> NoCheck) 
      then Illegal 
      else Legal (potential) 
    else Legal potential
  else Illegal

(** [is_valid_castle obj1 obj2 loc game] is whether castling with the given 
    objects and locations would be valid in [game]. DEPRECATED *)
let is_valid_castle obj1 obj2 loc game = 
  let proper_pos = (fst loc = 0 || fst loc = game.board_size) && 
                   (if game.turn = White then snd loc = 0 
                    else snd loc = game.board_size) in
  let king_pos = if game.turn = White then (4,0) else (4,game.board_size) in
  let in_between = (path king_pos loc) in 
  let path_bool = if in_between = [] then true 
    else (List.for_all 
            (fun loc -> (get_piece loc game) = None &&
                        check_checker {
                          game with
                          board = List.map 
                              (fun elt -> 
                                 if elt.color = game.turn &&
                                    elt.piece = King then 
                                   {elt with loc = loc} 
                                 else elt) 
                              game.board
                        } = NoCheck) (in_between)) in
  let rook_present = begin 
    match get_piece loc game with
    | None -> false
    | Some a -> a.piece = Rook 
  end in
  let first_turns = 
    (List.find (fun p -> p.piece = King && p.color = game.turn) 
       game.board).first_move && 
    (if rook_present then 
       (List.find (fun p -> p.piece = Rook && p.color = game.turn) 
          game.board).first_move else false) in
  if not proper_pos then (print_endline "not proper pos"; false) else
  if not path_bool then (print_endline "not good path"; false) else
  if not rook_present then (print_endline "not good rook prestn"; false) else
  if not first_turns then (print_endline "not good first turns"; false) 
  else true && proper_pos && path_bool && first_turns

(** [first_two lst] is the first two elements in [lst]. *)
let first_two lst = 
  match lst with
  | [] -> failwith "path not long enough in JSON"
  | h::[] -> failwith "path not long enough in JSON"
  | h::i::t -> [h; i]

(** [first lst] is the *first* element of a list. *)
let first lst = 
  match lst with
  | [] -> failwith "this cant happen"
  | a::_ -> a

(** [second lst] is the *second* element of a list. *)
let second lst = 
  match lst with
  | [] -> failwith "this cant happen"
  | a::[] -> failwith "this cant happen"
  | a::b::_ -> b

(** [valid_castle obj1 obj2 loc1 loc2 game] is whether castling with the given 
    objects and locations would be valid in [game]. *)
let valid_castle obj1 obj2 loc1 loc2 game =
  if string_to_piece obj1 = Rook then 
    let from_piece = get_piece loc1 game in
    let from_color = match from_piece with 
      | None -> failwith "this can't happen"
      | Some a -> a.color in
    let onto_piece = get_piece loc2 game in
    let onto_color = match onto_piece with 
      | None -> failwith "this can't happen"
      | Some a -> a.color in
    let obj1_first = match from_piece with 
      | None -> failwith "this can't happen"
      | Some a -> a.first_move in
    let obj2_first = match from_piece with 
      | None -> failwith "this can't happen"
      | Some a -> a.first_move in
    let rook_king = 
      match from_piece, onto_piece with
      | Some a, Some b -> begin if a.piece = Rook then (a.loc, b.loc) 
          else (b.loc, a.loc) end
      | _ -> failwith "this can't happen" in 
    let piece_bool = (kind_of_piece from_piece = string_to_piece obj1 &&
                      from_color = game.turn && onto_color = game.turn &&
                      kind_of_piece onto_piece = string_to_piece obj2) in
    let first_mov_bool = obj1_first&& obj2_first in
    let path = path (snd rook_king) (fst rook_king) in 
    let two_path = first_two path in 
    let two_fake_games = 
      [{game with board = 
                    (List.map 
                       (fun elt -> 
                          if elt.color = game.turn &&
                             elt.piece = King then 
                            {elt with loc = first two_path} 
                          else elt) 
                       game.board)};
       {game with board = 
                    (List.map 
                       (fun elt -> 
                          if elt.color = game.turn &&
                             elt.piece = King then 
                            {elt with loc = second two_path} 
                          else elt) 
                       game.board)}] in
    let current_check = if check_checker game = NoCheck then 1 else 0 in 
    let check_bool = List.length 
        (List.filter (fun elt -> NoCheck = check_checker elt) two_fake_games)
                     + current_check = 3 in
    (piece_bool && first_mov_bool && check_bool,
     (first two_path, second two_path))
  else (false, ((0,0),(0,0)))

let castle obj1 obj2 loc1 loc2 game = 
  let bool_new_locs = (valid_castle obj1 obj2 loc1 loc2 game) in 
  if fst (bool_new_locs) then
    Legal {
      game with
      board = game.board |> 
              List.map (fun p -> if p.loc = loc1 
                         then {p with loc = fst (snd bool_new_locs); 
                                      first_move = false} 
                         else if p.piece = King && p.color = game.turn 
                         then {p with loc = snd (snd bool_new_locs);
                                      first_move = false} 
                         else p);
    }
  else (print_endline "bad"; Illegal)

(** [valid_en_passant obj1 obj2 loc1 loc2 game] is whether an en passant
    with the supplied objects and locations is valid in [game]. *)
let valid_en_passant obj1 obj2 loc1 loc2 game = 
  let from_piece = (get_piece loc1 game) in
  let onto_piece = (get_piece loc2 game) in 
  let pawn_dying = 
    (get_piece (if game.turn = White then (fst loc2, snd loc2 - 1) 
                else (fst loc2, snd loc2 + 1)) game) in 
  if match from_piece, onto_piece with
    | Some a, None -> print_endline "d"; a.color <> game.turn
    | _ -> false
  then (false, (0,0))
  else
  if (kind_of_piece from_piece = string_to_piece obj1 &&
      Pawn = string_to_piece obj1 &&
      kind_of_piece pawn_dying = string_to_piece obj2 &&
      Pawn = string_to_piece obj2 &&
      (match pawn_dying with 
       | None -> failwith "this cant happen" 
       | Some pawn -> pawn.en_passant) &&
      (get_piece loc2 game) = None) then  
    match from_piece with
    | None -> (false, (0,0)) (*Never going to happen *) 
    | Some piece -> begin
        match piece.color with
        | White -> ((move_check_white piece.attack_moves loc1 loc2), 
                    match pawn_dying with 
                    | None -> failwith "this cant happen" 
                    | Some pawn -> pawn.loc)
        | Black -> ((move_check_black piece.attack_moves loc1 loc2),  
                    match pawn_dying with 
                    | None -> failwith "this cant happen" 
                    | Some pawn -> pawn.loc)
      end
  else ( print_endline "e"; (false, (0,0)))

let passant obj1 obj2 loc1 loc2 game = 
  let bool_new_locs = valid_en_passant obj1 obj2 loc1 loc2 game in 
  if fst (bool_new_locs) then
    Legal {
      game with
      board = 
        game.board |> 
        List.fold_left (fun acc p-> if p.loc = loc1 
                         then {p with loc = (loc2); 
                                      first_move = false}::acc
                         else if p.loc = (snd bool_new_locs)
                         then acc
                         else p::acc) [];
    }
  else (print_endline "bad"; Illegal) 
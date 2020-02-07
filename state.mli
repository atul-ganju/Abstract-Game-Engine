(**
   Represents dynamic information about a board game

   This represents both data loaded from json files, such as initial pieces,
   loading from JSON
*)

exception IllegalPosition

(** the type representing the various piece categories. *)
type piece_name =  Knight | Pawn | Bishop | Rook | Queen | King | Checker

(** the type representing a player's check(mate) status. *)
type check = NoCheck | Check | CheckMate

(** the type representing different teams/players. *)
type color = Black | White

(** the type corresponding to a location on a game board. *)
type location = (int*int)

(** the type corresponding to a displacement on a game board. *)
type move = (int*int)

(** the type corresponding to a string represention of a piece name. *)
type obj = string

(** the type representing a general piece in a game, 
     including static and dynamic information. *)
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

(** the type representing a general game, 
    including static and dynamic information. *)
type t = {
  board : piece list;
  board_size : int;
  turn : color;
  description : Yojson.Basic.t;
  rules : string list;
}

(** the type representing an attempted move in a general game, either a 
    failed attempt or the successful game having executed the move. *)
type result = Legal of t | Illegal

(** [init_state j] is the game that [j] represents.
    Requires: [j] is a valid JSON representation of a game. *) 
val init_state : Yojson.Basic.t -> t

(** [piece_loc p] is the location of [p]. *)
val piece_loc : piece -> location

(** [piece_color p] is the color of [p]. *)
val piece_color : piece -> color

(** [get_piece loc game] is the Some p at [loc] in [game], or None if 
    no piece is present there.*)
val get_piece : location -> t -> piece option

(** [is_valid_move obj loc1 loc2 game] is whether a move with
    the given [obj]  would be valid when going from [loc1] to [loc2] 
    in [game]. *)
val is_valid_move : obj -> location -> location -> t -> bool

(** [is_valid_take obj1 obj2 loc1 loc2 game] is the boolean result of trying to 
    take a piece of type [obj2] at [loc2] with a piece of [obj1] at 
    [loc1] in [game]. *)
val is_valid_take : obj -> obj -> location -> location -> t -> bool

(** [take obj1 obj2 loc1 loc2 game] is the result of attempting taking a piece
    of type [obj2] at [loc2] with a piece of [obj1] at [loc1] in [game]. *)
val take : obj -> obj -> location -> location -> t -> result

(** [move obj loc1 loc2 game] is the result of attempting a move of a piece of
    corresponding type [obj] from [loc1] to [loc2] in [game]. *)
val move : obj -> location -> location -> t -> result

(** [castle obj1 obj2 loc1 loc2 game] is the result of attempting a castle with
    between [obj1] on [loc1] and [obj2] on [loc2]*)
val castle : obj -> obj -> location -> location -> t -> result

(** [win_condition game] is whether the [game] currently 
    satisfies victory conditions.*) 
val win_condition : t -> bool

(** [piece_to_string piece] is the string corresponding to [p]'s 
    piece type. *) 
val piece_to_string : piece -> string

(** [get_board_size game] is the size of the square board of [game]. *)
val get_board_size : t -> int

(** [kind_of_piece P] is the piece type [P] contains. Fails if P is None. *)
val kind_of_piece : piece option -> piece_name

(** [json_of_piece p] is a json string representation of [p]. *)
val json_of_piece : piece -> string

(** [json_of_board game] is a json string representation of [game], including
    all rules and current piece locations. *)
val json_of_board : t -> string

(** [piece_color_to_string p] is the string representation of [p]'s color. *)
val piece_color_to_string : piece -> string

(** [add_piece_of_json name loc color json] is the piece constructed from [json]
    with the specified [name] [loc] and [color]. *)
val add_piece_of_json : piece_name -> location -> 
  color -> Yojson.Basic.t -> piece

(** [string_to_piece str] is the piece name type corresponding to [str]
    e.g. "knight" -> Knight.
    fails if does not correspond to any name. *)
val string_to_piece : string -> piece_name

(** [piece_move_options p game] is the list of valid moves of [p] in [game],
    containing the absolute final and initial locations. *)
val piece_move_options : piece -> t -> (int*int*location) list

(** [check_checker game] is whether a player is in check(mate) in [game]. *)
val check_checker : t -> check

(** [passant obj1 obj2 loc1 loc2 game] is the result of attempting an
    en passantat with [obj1] on [obj2] [obj1] being on [loc1] and [obj2] being
    on [loc2]*)
val passant : obj -> obj -> location -> location -> t -> result

(** [path loc1 loc2] is the list of locations on a board between [loc1] and
    [loc2] if the two are connected by a line (i.e. horizontal, vertical,
    or diagonal separation). *)
val path : location -> location -> location list

(** [piece_take_options2 p game] is the list of valid takes of [p] in [game],
    containing the absolute final and initial locations. *)
val piece_take_options_2 : piece -> t -> (int*int) list

(** [checker_options piece game] is [piece_take_options piece game] applied
    to checkers. *)
val checker_options : piece -> t-> (int*int) list

(** [fst_snd_three (a,b,c)] is the first two elements of [(a,b,c)]. *)
val fst_snd_three : int*int*int -> int*int

(** [trd_three (a,b,c)] is the third element of [(a,b,c)]. *)
val trd_three : int*int*int -> int
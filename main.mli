(** 
   The main entry point for the game, consisting of both the game loop
   and game file loading.
*)

open State

(** [play_game dir] creates a game from within [dir] and starts the gameplay
    with it. *)
val play_game : string -> unit

(** [play_the_rest game dir] is the main repl loop of [game] generated from 
    within [dir]. *)
val play_the_rest : State.t -> string -> unit

(** [main ()] prompts for the game to play, then starts it. *)
val main : unit -> unit
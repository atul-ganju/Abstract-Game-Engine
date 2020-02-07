(** 
   Generates text graphics to represent game state.
*)

open State

(** [print_board game] outputs to standard output a text-graphics 
    representation of [game]. *)
val print_board : State.t -> unit
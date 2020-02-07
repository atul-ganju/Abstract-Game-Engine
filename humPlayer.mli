(**
   Represents a human player and their input.
*)

(** [turn game] produces a user generated turn command. *)
val turn : State.t -> string

(** 
   Represents types of game commands and performs 
   parsing of player input commands.
*)

open State

(** type representing a generic game command *)
type command = 
  | Move of (obj * location * location)
  | Quit
  | Save
  | Take of (obj * obj * location * location)
  | Replace of obj
  | Help
  | Castle of (obj * obj * location * location)
  | EnPassant of (obj * obj * location * location)
  | Cancel

(** exception signalling empty input *)
exception Empty

(** exception signalling malformed input *)
exception Malformed of string

(** [explode str] converts a string input to its corresponding list of
    characters. e.g. 'e5' -> ['e';'5']. *)
val explode : string -> char list

(** [coordinate input game] is the numeric position corresponding to natural
    player input e.g. b2 corresponds to (1,1). *)
val coordinate : char list -> State.t -> location

(** [parse str game] parses a player's input into a command *)
val parse : string -> State.t -> command
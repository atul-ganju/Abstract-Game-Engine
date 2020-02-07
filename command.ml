open State

type game = string

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

exception Empty

exception Malformed of string

let explode str = 
  List.init (String.length str) (String.get str)

let coordinate lst game = 
  match lst with
  | char::number::[] -> 
    let ascii_char = Char.code char in 
    let ascii_number = Char.code number in 
    if (97 <= ascii_char && ascii_char <= 122) then 
      if ((Char.code number - (Char.code '0')) 
          <= (State.get_board_size game)) then 
        (ascii_char - 97, ascii_number - 49) 
      else raise (Malformed "entered move outside of range")
    else raise (Malformed "not proper char")
  | _ -> raise (Malformed "you did not enter a valid move")

let parse string game =
  let s = String.lowercase_ascii string in
  match s |> String.split_on_char ' ' |> List.filter (fun s -> s <> "") with
  | [] -> raise Empty
  | "quit"::"game"::[] -> Quit
  | "quit"::_ -> raise (Malformed "you did not enter a valid command")
  | "save"::"game"::[] -> Save
  | "save"::_ -> raise (Malformed "you did not enter a valid command")
  | "take"::obj1::"on"::loc1::"with"::obj2::"on"::loc2::[] ->
    Take (obj2, obj1, coordinate (explode loc2) game, 
          coordinate (explode loc1) game)
  | "replace"::"pawn"::"with"::obj::[] -> Replace obj
  | "replace"::_ -> raise (Malformed  "you did not enter a valid command")
  | "move"::obj::"on"::loc1::"to"::loc2::[] -> 
    Move (obj, coordinate (explode loc1) game, coordinate (explode loc2) game)
  | "help"::[] -> Help
  | "help"::_ -> raise (Malformed "you did not enter a valid command")
  | "castle"::obj1::"on"::loc1::"with"::obj2::"on"::loc2::[] -> 
    Castle (obj1, obj2, coordinate (explode loc1) game, 
            coordinate (explode loc2) game)
  | "en"::"passant"::obj1::"on"::dead::"with"::obj2::"on"::
    live::"moving"::"it"::"to"::empty::[] ->
    EnPassant (obj2, obj1, coordinate (explode live) game, 
               coordinate (explode empty) game)
  | "cancel"::[] -> Cancel
  | _ -> raise (Malformed "you did not enter a valid command")
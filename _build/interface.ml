open State

(** [print_alphabet game] prints A..A+board game size. *)
let print_alphabet game = 
  let board_size = State.get_board_size game in
  for i = 0 to (board_size - 1) do
    if (i = 0) then print_string "     A"
    else if (i = board_size -1) 
    then print_endline ("       "^Char.escaped (Char.chr (65+i))^"   ")
    else print_string ("       "^Char.escaped (Char.chr (65+i)))
  done

(** [print_dashed_line game] prints a dashed line for each row of [game]. *)
let print_dashed_line game = 
  let board_size = State.get_board_size game in
  for i = 0 to (board_size - 1) do
    if (i = 0) then print_string "  -------"
    else if (i = board_size -1) 
    then print_endline (" ------- ")
    else print_string (" -------")
  done

(** [print_first_line game] is the first line of each row of [game]. *)
let print_first_line game i = 
  let board_size = State.get_board_size game in 
  for j = 0 to (board_size - 1) do 
    let string =
      match State.get_piece (j, (get_board_size game) - i-1) game with
      | None -> if (((i+j) mod 2) = 1) then "       " else " . . . "
      | Some piece -> begin 
          match piece.piece with
          | Pawn -> if (piece.color = Black) then "   _   " else "   _   "
          | Rook -> if (piece.color = Black) then " [___] " else " [@@@] "
          | Knight -> if (piece.color = Black) then "  /^)  " else "  /^)  "
          | Bishop -> if (piece.color = Black) then "  .O.  " else "  .O.  "
          | Queen -> if (piece.color = Black) then " \\o^o/ " else " \\o^o/ "
          | King -> if (piece.color = Black) then " __+__ " else " __+__ "
          | Checker -> if (piece.color = Black) then " O   O " else " X   X "
        end in 
    if (j = 0) then print_string (" |"^string)
    else if (j = board_size -1) then print_endline ("|"^string^"|")
    else print_string ("|"^string)
  done

(** [print_second_line game] is the second line of each row of [game]. *)
let print_second_line game i = 
  let board_size = State.get_board_size game in 
  for j = 0 to (board_size - 1) do 
    let string =
      match State.get_piece (j, (get_board_size game) - i-1)  game with
      | None -> if (((i+j) mod 2) = 1) then "       " else " . . . "
      | Some piece -> begin 
          match piece.piece with
          | Pawn -> if (piece.color = Black) then "  ( )  " else "  (@)  "
          | Rook -> if (piece.color = Black) then "  [ ]  " else "  [@]  "
          | Knight -> if (piece.color = Black) then "   )(  " else "   d(  "
          | Bishop -> if (piece.color = Black) then "  \\ /  " else "  \\@/  "
          | Queen -> if (piece.color = Black) then "  [ ]  " else "  [@]  "
          | King -> if (piece.color = Black) then " `. .' " else " `d|b' "
          | Checker -> 
            if (piece.color = Black) then 
              begin if piece.kinged then "  |+|  " 
                else "   O   " end 
            else
              begin if piece.kinged then "  |+|  " else "   X   " 
              end
        end in 
    if (j = 0) 
    then print_string ((string_of_int ((get_board_size game) - i))^"|"^string)
    else if (j = board_size -1) 
    then print_endline 
        ("|"^string^"|"^(string_of_int ((get_board_size game) - i)))
    else print_string ("|"^string)
  done

(** [print_third_line game] is the third line of each row of [game]. *)
let print_third_line game i = 
  let board_size = State.get_board_size game in 
  for j = 0 to (board_size - 1) do 
    let string =
      match State.get_piece (j, (get_board_size game) - i-1) game with
      | None -> if (((i+j) mod 2) = 1) then "       " else " . . . "
      | Some piece -> begin 
          match piece.piece with
          | Pawn -> if (piece.color = Black) then "  /_\\  " else "  /A\\  "
          | Rook -> if (piece.color = Black) then " /___\\ " else " /@@@\\ "
          | Knight -> if (piece.color = Black) then "  <__> " else "  <@@> "
          | Bishop -> if (piece.color = Black) then "  /_\\  " else "  /A\\  "
          | Queen -> if (piece.color = Black) then " /___\\ " else " /@@@\\ "
          | King -> if (piece.color = Black) then " /___\\ " else " /@@@\\ "
          | Checker -> if (piece.color = Black) then " O   O " else " X   X "
        end in 
    if (j = 0) then print_string (" |"^string)
    else if (j = board_size -1) then print_endline ("|"^string^"|")
    else print_string ("|"^string)
  done


let print_board game = 
  print_alphabet game;
  let board_size = State.get_board_size game in 
  for i = 0 to (board_size-1) do
    print_dashed_line game;
    print_first_line game i;
    print_second_line game i;
    print_third_line game i;
  done;
  print_dashed_line game;
  print_alphabet game;

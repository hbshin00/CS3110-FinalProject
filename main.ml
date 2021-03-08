open Cards
open State
open Ai

(** The type of commands that the user and the NPCs can make for betting
    functions. *)
type bet_cmd = Fold | Raise_bet | Call | Check | All_in

(** [printer_helpers money_needed user] prints several statements for the user
    before they bet, letting the user know what options are available *)
let printer_helpers money_needed user =
  let chck = (if money_needed = 0 then ("check", "'K'") else ("call","'C'")) in
  print_endline ("Would you like to fold, raise, " ^ 
                 fst chck ^ ", or go all-in?");
  print_endline ("Please type in 'F', 'R', " ^ 
                 snd chck ^ ", or 'A' respectively.\n");
  if fst chck = "call" then
    if money_needed <= get_money user then
      print_endline ("You need to bet at least " ^ string_of_int money_needed 
                     ^ " moneys to stay in")
    else
      print_endline ("You must go all-in with " ^ string_of_int (get_money user) 
                     ^ " moneys to stay in")

(** [read_bet t bool] prompts the user with a string, reads the user's input,
    and performs an action ([fold], [raise_bet], [call], [check], or [all_in])
    based on the character input by the user. [bool] is true if the player
    inputs an empty string, in which case the user is reprompted  *)
let rec read_bet' state bool =
  let user = Array.get (get_player_array state) 0 in
  let money_needed = get_hrb state - get_money_bet user in
  if bool = true then
    printer_helpers money_needed user;
  print_string "> ";
  let bet = read_line () in
  get_bet bet state

and get_bet bet state =
  match bet |> String.lowercase_ascii |> String.trim with
  |"f" | "fold" -> (fold 0 state, false)
  |"r" | "raise" -> begin print_endline "How much would you like to raise?\n";
      print_string  "> ";
      let amt = read_line () |> int_of_string in
      try ((raise_bet 0 amt state), false) with
      | NotEnoughMoney "Can't raise, not enough money" ->
        print_endline "Not enough money; try again: ";
        read_bet' state true end
  | "c" | "call" -> begin try (call 0 state, false) with
      | InvalidBet "Can't call" -> print_endline "Can't call; try again";
        read_bet' state true end
  | "k" | "check" -> begin try (check 0 state, false) with
      | InvalidBet "Can't check" -> print_endline "Can't check; try again";
        read_bet' state true end
  | "a" |"all-in" | "allin" | "all in" -> (all_in 0 state, false)
  | "" -> read_bet' state false
  | "quit" -> (state, true)
  | _ -> print_endline "Invalid input. \n"; read_bet' state true

(** [read_bet state] calls [read_bet' state true] as a helper function, so that
    it's more succinct every time we call [read_bet'] *)
let read_bet state =
  read_bet' state true

(** [pp_card c] pretty-prints card [c] with face cards given their proper
    names. *)
let pp_card (c : card) =
  let number = get_number c in match number with
  | 14 -> "\"Ace of " ^ (get_suit c) ^ "\""
  | 13 -> "\"King of " ^ (get_suit c) ^ "\""
  | 12 -> "\"Queen of " ^ (get_suit c) ^ "\""
  | 11 -> "\"Jack of " ^ (get_suit c) ^ "\""
  | _ -> "\"" ^ (number |> string_of_int) ^ " of " ^ (get_suit c) ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
        if n = 100 then acc ^ "..."  (* stop printing long list *)
        else loop (n + 1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

(** [show_cards cards prompt] shows a list of cards [cards] to the user as a
    string, following the string [prompt], which is intended to tell the user
    what cards are being shown. *)
let show_cards cards prompt =
  print_endline (prompt ^ (cards |> pp_list pp_card) ^ "\n")

(** [show_comm_cards t rnd] shows the community cards to the user. Returns unit
    if this is called at a time that does not correspond with any new community
    cards being dealt (i.e., if [rnd] ≠ 1, 2, or 3, where 1, 2, and 3 correspond
    to flop, turn, and river, respectively). *)
let show_comm_cards state round =
  if round = 1 then show_cards (get_flop state) "Flop: "
  else if round = 2 then show_cards ((get_flop state) @ (get_turn state))
      "Turn: "
  else if round = 3 then show_cards ((get_river state) @ (get_flop state) @
                                     (get_turn state)) "River: "
  else ()


let rec get_total_pot sides acc_pot = match sides with
  | h :: t -> get_total_pot t ((fst h) + acc_pot)
  | [] -> acc_pot

(** [show_info t rnd p] shows the user [player] his or her [pocket] and [money],
    and shows the [pot] of state [t]; it also shows the user the community 
    cards, which can be [flop], [turn], or [river], depending on the input 
    [round]
    Requires: [round] is an int in 1..3, where 1, 2, and 3 represent the rounds
    [flop], [turn], and [river], respectively. *)
let show_info state round player =
  show_cards (get_pocket player) "Your cards: ";
  print_endline ("\tYour money: " ^ (get_money player |> string_of_int) ^ "\n");
  let sides = get_side_pots state in let pot = get_total_pot sides 0 in
  print_endline ("\tPot: " ^ (pot |> string_of_int) ^ "\n");
  show_comm_cards state round

(** [get_user s] returns the first player in the array, which is the user's
    player *)
let get_user state = Array.get (get_player_array state) 0

(** [show_decision state decision amount pos] shows the bet, fold, etc.
    for player in position [pos] in the player array in state [state]. *)
let show_decision state decision amt pos =
  let players = get_player_array state in
  let player = Array.get players pos in
  match decision with
  | Fold -> print_endline ("\t" ^(get_name player) ^ " folded\n");
  | Raise_bet -> print_endline ("\t" ^(get_name player) ^ " raised " ^
                                (string_of_int amt)^ "\n");
  | Call -> print_endline ("\t" ^(get_name player) ^ " called\n");
  | Check -> print_endline ("\t" ^(get_name player) ^ " checked\n");
  | All_in -> print_endline ("\t" ^(get_name player) ^ " went all-in with " ^
                             (string_of_int amt)^ "\n")

(** [do_cpu_bet state decision amount position] calls the betting function
    corresponding to the bet_cmd [decision], and returns the state for this
    betting function called on the player in position [position] in the array.
    [amount] is the amount raised if [decision] is [Raise], and 0 otherwise *)
let do_cpu_bet state decision amount position =
  match decision with
  | Fold -> (fold position state, Fold)
  | Raise_bet -> begin try (raise_bet position amount state, Raise_bet) with
      | NotEnoughMoney "Can't raise, not enough money" -> 
        (call position state, Call)
    end
  | Call -> begin try (call position state, Call) with
      | InvalidBet "Can't call" -> (check position state, Check) end
  | Check -> begin try (check position state, Check) with
      | InvalidBet "Can't check" -> (fold position state, Fold) end
  | All_in -> (all_in position state, All_in)

(** [show_all_money players pos] shows the money left for all players in the
    player array [players], starting with the player in position [pos] in the
    array. *)
let rec show_all_money players pos = if pos = Array.length players then ()
  else begin print_endline (get_name (Array.get players pos) ^ "'s money: " ^
                            (get_money (Array.get players pos)
                             |> string_of_int)^ "\n");
    show_all_money players (pos + 1); () end

(** returns true if more than one player betting left and false otherwise
    (rest have folded or gone all in) *)
let rec check_left players acc acc_left =
  if acc = Array.length players then acc_left > 1
  else let player = Array.get players acc in
    if get_money player = 0 || get_folded player
    then check_left players (acc + 1) acc_left
    else check_left players (acc + 1) (acc_left + 1)

(** [make_decisions_aux pocket trait table num_players money] makes a betting
    decision for a player by calling one of the [odds] functions, based on how
    many cards have been shown so far.
    [trait] is a string denoting the aggression of the player betting:
    "cons" = conservative, "aggr" = aggressive *)
let make_decision_aux pocket trait table num_players money : int * int =
  let mc_tuple = monte_carlo pocket table num_players in
  let win_odds = odds "win" mc_tuple in
  let len = List.length table in
  if len = 0 then pocket_odds trait win_odds
  else if len = 3 then flop_odds trait win_odds
  else if len = 4 then turn_odds trait win_odds
  else if len = 5 then river_odds trait win_odds
  else failwith "make_decision_aux: invalid table length"

(** [make_decision state player position bool round] makes a betting decision
    for the NPCs based on their odds of winning, based on their current
    knowledge about the deck. *)
let make_decision state player position bool round = let open Array in
  let decisions = [|Fold; Raise_bet; Call; Check; All_in|] in
  let table = begin if (round = 0) then [] else if round = 1 then get_flop state
    else if round = 2 then (get_flop state) @ (get_turn state) else
      (get_flop state) @ (get_turn state) @ (get_river state) end in
  let pocket = get_pocket player in let len = length (get_player_array state) in
  let decision_tuple = make_decision_aux pocket "cons" table len 
      (get_money player) in
  let decision = get decisions (fst decision_tuple) in 
  let player = get (get_player_array state) position in
  let diff = (get_hrb state - get_money_bet player) in
  let amount = 
    if decision = Raise_bet then (get_money player) / (snd decision_tuple) else 
      get_money player in if get_still_betting player then 
    let decision = if decision = Check && diff > 0 then Fold else decision in
    let tup = do_cpu_bet state decision amount position in
    (fst tup, show_decision state (snd tup) amount position)
  else if bool then let next = (position + 1) mod len in
    ((quick_update state (get_pot state) (get_hrb state)
        (get_player_array state) (next)), ()) else (state, ())

(** [should_bet players player hrb] returns true if player [player] should be
    betting this round, based on whether they have folded, whether they need to
    bet more money to stay in, and whether there are other players who are still
    betting (i.e., if everyone else has 0 money or has folded, and this player
    has already bet the highest round bet, then they should not bet) *)
let should_bet players player hrb =
  if (get_money_bet player < hrb && get_money player > 0 &&
      (get_folded player = false))
  then true
  else check_left players 0 0

(** [run_npc_bets t pos] makes decisions for all the npc players in the array
    [players] in state [t], starting with position [pos], which should be 1 to
    start because [Array.get players 0] is the user's player (as of now). *)
let rec run_npc_bets state position round =
  let players = get_player_array state in
  if position = Array.length players then state
  else
    let player = Array.get players position in
    if should_bet players player (get_hrb state) then
      let new_state = fst (make_decision state (Array.get players position)
                             position false round) in
      run_npc_bets new_state (position + 1) round 
    else run_npc_bets state (position + 1) round

(** [reveal_cards players pos] reveals all of the cards of the npc players to
    the user *)
let rec reveal_cards players position state =
  if position = Array.length players then ()
  else let player = Array.get players position in
    if get_folded player then ()
    else
      let cards = (get_flop state) @ (get_turn state) @ (get_river state) in
      let hand = best_hand cards (get_pocket player) in
      print_endline ("\t" ^get_name player ^ "'s hand: " ^ (pp_hand hand));
      reveal_cards players (position + 1) state

(** [check_if_betting_done state list] returns true if the betting is done for
    this round of betting, and returns false otherwise.  *)
let rec check_if_betting_done state = function
  | [] -> true 
  | h :: t -> if get_money h <> 0 && get_folded h = false
    then
      get_money_bet h = get_hrb state && check_if_betting_done state t
    else
      check_if_betting_done state t

(** [end_round state round] handles the betting after everyone in the array
    after everyone has already bet once. Returns the state once the round of
    betting is over. *)
let rec end_round state round =
  let players = get_player_array state in
  let lst = Array.to_list players in
  Unix.sleep 2;
  let ended = check_if_betting_done state lst in
  if ended
  then check_all_ins state
  else begin
    let betting = get_player_betting state in
    end_betting state round players betting
  end

(** [end_betting state round players betting] handles the actual betting part of
    [end_round state round] by checking if each player should be betting, and
    then carries out their bets if they should, and skips them otherwise. *)
and end_betting state round players betting = let open Array in
  if betting = 0
  then if (get_folded (get_user state)) = true
       || (get_money (get_user state)) <= 0
    then let new_state = quick_update state (get_pot state) (get_hrb state)
             (get_player_array state) 1 in
      end_round new_state round
    else let new_state = read_bet state in
      end_round (fst new_state) round
  else
  if should_bet players (get players betting) (get_hrb state) then
    let new_state =
      fst (make_decision state (get players betting) betting true round ) in
    end_round new_state round
  else end_betting state round players ((betting + 1) mod (length players))

(** [pp_tuple tuple] pretty-prints a person * hand tuple as a name * hand
    tuple *)
let pp_tuple tuple = "(" ^ (get_name (fst tuple)) ^ ", " ^ (pp_hand (snd tuple))

(** [read_name ()] prompts the user for a name, and returns the input if the
    input is valid, and reprompts them otherwise *)
let rec read_name () =
  let given_name = read_line () |> String.trim in
  if given_name = "" then begin
    print_string ("> ");
    read_name () end
  else given_name

(** [read_name ()] prompts the user for a name, and returns the input if the
    input is valid, and reprompts them otherwise *)
let rec get_npcs () =
  let input_num = read_line () |> String.trim in match input_num with
  | "1" | "2" | "3" | "4" | "5" | "6" | "7" | "8" -> int_of_string (input_num)
  | _ -> print_string ("> "); get_npcs ()


let rec prompt_money bool = 
  let _ =
    if bool = false then
      begin
        print_endline "\nPlease enter the amount of money you'd like to start with.";
        print_endline ("\nAll NPC's will start with the same amount. \n"); 
      end
    else ()
  in
  print_string("> ");
  let money = read_line () |> String.trim |> int_of_string_opt in
  match money with 
  | None -> print_endline ("\nNot a valid money value, please try again.\n"); 
    prompt_money true
  | Some int -> int

(** [init_main] initializes the game by prompting the user with intro
    messages, and creating the player array based on the user's inputs. *)
let init_main () =
  print_endline "\nWelcome to Texas Hold'em Poker! \n";
  print_endline "Please enter your name. \n";
  print_string  "> ";
  let name = read_name () in
  print_endline
    "\nPlease enter the number of NPC's you would like to play with.";
  print_string "\nYou can play with 1 to 8 other NPC's. \n \n> ";
  let npc = get_npcs () in
  let money = prompt_money false in
  let player1 = player_init name money in
  player_array_make_cpus npc player1

(** [run_ftr state round] calls the bets for the flop, turn, or river round of
    betting, based on the input of [round]. If [round = 0], then   *)
let run_ftr state round =
  let new_state = run_npc_bets state 1 round in
  let newer_state = end_round new_state 1 in
  show_all_money (get_player_array newer_state) 0;
  show_info newer_state (round + 1) (get_user newer_state);
  newer_state

(** [det_still_betting state] returns (state', bool), where [state'] is the new
    state after the player has bet if the player is still betting, and [bool] is
    true, or [state'] is state and [bool] is false *)
let det_still_betting state =
  if get_folded (get_user state) = false &&
     (get_money (get_user state)) > 0
  then read_bet state
  else (state, false)

(** [game_over players acc acc_left] returns true if only one player has money
    left, and false otherwise. *)
let game_over players =
  if get_money (Array.get players 0) = 0 then true else
    let rec game_over players acc acc_left =
      if acc = Array.length players then true
      else let player = Array.get players acc in
        if get_money player = 0 then game_over players (acc + 1) acc_left
        else if acc_left > 0 then false else
          game_over players (acc + 1) (acc_left + 1)
    in game_over players 0 0

(** [show_winners init final] prints the players who have gained money this
    round, along with the amount they have won.  *)
let rec show_winners init_players final_players =
  match init_players, final_players with
  | h1 :: t1, h2 :: t2 ->
    let diff = get_money h2 - get_money h1 in
    if diff > 0 then print_endline (get_name h1 ^ " won " ^ 
                                    string_of_int diff ^ " moneys");
    show_winners t1 t2
  | _ -> ()

(** [show_lowers init final] prints the players who have lost money this
    round, along with the amount they have lost.  *)
let rec show_losers init_players final_players = 
  match init_players, final_players with
  | h1 :: t1, h2 :: t2 ->
    let diff = get_money h1 - get_money h2 in
    if diff > 0 then print_endline (get_name h1 ^ " lost " ^ 
                                    string_of_int diff ^ " moneys");
    show_losers t1 t2
  | _  -> ()

(** [restart_state state bool] returns the state for the start of a hand,
    which depends on whether this is the first hand of the game
    If [first] is false, then this is not the first round, so [state] must be
    updated by reseting players' pockets and reshuffling/dealing the deck. *)
let restart_state state first =
  if first then let _ = print_endline "\nThe game is now beginning." in
    state
  else let _ = print_endline "\nThe next hand is now beginning." in
    let players = get_player_array state in
    reset_players players;
    let shuffled_deck = shuffle std_deck in
    let deck_remainder = deal shuffled_deck players 0 in
    init_state deck_remainder players

(** [show_final_info state start] shows the user the NPC's pocket, shows the
    sorted list of players' hands along with their respective hands,  *)
let show_final_info state start =
  let cards = (get_flop state) @ (get_turn state) @ (get_river state) in
  let sorted = get_hands (get_player_array state) cards in
  reveal_cards (get_player_array state) 0 state;
  print_endline ("Hands: " ^ (pp_list pp_tuple sorted));
  let end_state = distribute state (get_side_pots state) in
  let players_end = (get_player_array end_state) |> Array.to_list in
  show_winners start players_end;
  show_losers start players_end;
  end_state

(** [run_game ()] runs one session of a poker game by initializing a player
    array, dealing the deck, and calling [run_hand] on the new state. *)
let rec run_game () =
  let player_array = init_main () in
  let shuffled_deck = shuffle std_deck in
  let deck_remainder = deal shuffled_deck player_array 0 in
  let default_state = init_state deck_remainder player_array in
  run_hand default_state true

(** [run_hand state bool] runs one hand of a poker game by running pocket,
    flop, turn, and river betting. Recursively calls when the user starts a new
    hand.  *)
and run_hand state first = begin
  let state' = restart_state state first in
  let players_start = (get_player_array state') |> Array.to_list in
  print_endline "You may input 'quit' at any time to quit the game. \n";
  show_cards (Array.get (get_player_array state') 0 |> get_pocket)
    "\tYour pocket: ";
  let new_state_tuple = read_bet state' in
  if snd new_state_tuple then () else
    let new_state = run_ftr (fst new_state_tuple) 0 in
    let flop_tuple = det_still_betting new_state in
    if snd flop_tuple then () else
      let flop_state = run_ftr (fst flop_tuple) 1 in
      let turn_tuple = det_still_betting flop_state in
      if snd turn_tuple then () else
        let newer_state5 = run_ftr (fst turn_tuple) 2 in
        let new_state6_tuple = det_still_betting newer_state5 in
        if snd new_state6_tuple then () else
          let end_state = run_ftr (fst new_state6_tuple) 3 in
          end_of_game (show_final_info end_state players_start) end

(** [end_of_game state] handles the case when a round has ended by:
    –telling the user if they won or lost, (if the game is over), and then
    giving them the option to restart or quit, or
    –letting the user start the next hand if a hand has just ended, but the game
    has not ended yet. *)
and end_of_game state =
  let players = get_player_array state in
  match
    if game_over players then
      if get_money (Array.get players 0) > 0 then "win" else "lose"
    else "not done" with
  | "win" -> end_message "won!"
  | "lose" -> end_message "lost :("
  | _ -> continue_message state

(** [end_message msg] prints a message to the user at the end of a game, letting
    them know whether they won or lost, and prompting them with the choice to 
    start a new game or quit. *)
and end_message msg =
  print_endline ("You " ^ msg);
  print_endline ("\nType 'quit' to quit, or press any other key to play again");
  print_string ("> ");
  match read_line () |> String.lowercase_ascii |> String.trim with
  | "q" | "quit" -> ()
  | _ -> run_game ()

(** [continue message state] prints a message to the user at the end of a hand
    when the game is not over yet, and then gives the player the option to 
    continue to the next hand, or quit.  *)
and continue_message state =
  begin print_endline ("\nHand over; type 'quit' to quit or press any other key to start the next hand\n");
    print_string "> ";
    match read_line () |> String.lowercase_ascii |> String.trim with
    | "q" | "quit" -> ()
    | _ -> run_hand state false end

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  run_game ()

(* Execute the game engine. *)
let () = main ()



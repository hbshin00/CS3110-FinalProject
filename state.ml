open Cards
exception NotEnoughMoney of string
exception InvalidBet of string

type t = {
  dealer : int;
  flop : card list;
  turn : card list;
  river : card list;
  pot : int; (** Only for current round *)
  high_round_bet : int; (** The maximum amount of money a player has bet in the
                            current round*)
  player_array : player array;
  player_betting : int; (** position of player currently betting *)
  side_pots : (int * string list) list;
}

let get_dealer state = state.dealer
let get_flop state = state.flop
let get_turn state = state.turn
let get_river state = state.river
let get_pot state = state.pot

let get_hrb state = state.high_round_bet
let get_player_array state = state.player_array
let get_player_betting state = state.player_betting
let get_side_pots state = state.side_pots

(** [init_helper] designates the flop, turn, and river in the deck by returning
    a tuple of type (card list * card list * card list).
    [deck] can be shuffled or unshuffled. *)
let get_ftr_tuple deck =
  match deck with
  | hd1 :: hd2 :: hd3 :: hd4 :: hd5 :: tl ->
    (hd1 :: hd2 :: hd3 :: [], [hd4], [hd5])
  | _ -> failwith "not enough cards to designate ftr"

(** [init_helper_helper ftr element] returns the flop, turn, or river, based
    on the input [element].
    Requires:
     [ftr]: a tuple (x, y, z) where [x] is a card list of length 3, [y] and [z]
       are card lists of length 1
     [element]: int in 1..3 *)
let get_ftr (ftr : 'a list * 'a list * 'a list) element  =
  match ftr with
  | (f, t, r) ->
    if element = 1 then f
    else if element = 2 then t
    else if element = 3 then r
    else failwith "element input invalid" (* Maybe change to an exception *)

(** [update_state] returns a new state with the changes to [state]*)
let update_state dealer fp tn rvr cp hrb pa pb sps=
  {dealer = dealer;
   flop = fp;
   turn = tn;
   river = rvr;
   pot = cp;
   high_round_bet = hrb;
   player_array = pa;
   player_betting = pb;
   side_pots = sps
  }

let quick_update (state : t) cp hrb pa pb : t=
  update_state state.dealer state.flop state.turn state.river cp hrb
    pa pb state.side_pots

let side_pot_update (state : t) sp =
  update_state state.dealer state.flop state.turn state.river state.pot
    state.high_round_bet state.player_array
    state.player_betting sp

let init_state deck player_array=
  let ftr = get_ftr_tuple deck in
  let player_list = player_array|> Array.to_list |> List.map get_name in
  update_state 0 (get_ftr ftr 1)
    (get_ftr ftr 2) (get_ftr ftr 3) 0 0 player_array 1 [(0, player_list)]

let reset_players player_array = let open Array in
  for i = 0 to Array.length player_array - 1 do
    let player = get player_array i in
    let name = get_name player in let money = get_money player in
    let new_player = player_init name money in
    set player_array i new_player
  done

(** [bank_of_names] represents the names from which we choose NPC names when
    creating a player array. *)
let bank_of_names = [|"dummy";"Abhyuday"; "Dhruv"; "Hali"; "Gries";
                      "Simon"; "Penelope"; "Egg"; "Alvin"; "Theodore"|]

let player_array_make_cpus (number_cpus : int) (player1 : player)
  : player array = let open Array in
  let init = make (1 + number_cpus) player1 in
  for i = 1 to number_cpus do
    let player_name = get_name player1 |> String.lowercase_ascii in
    let cpu_name = get bank_of_names i  |> String.lowercase_ascii in
    let name =
      if player_name = cpu_name then
        get bank_of_names (length bank_of_names - 1)
      else get bank_of_names i in
    set init i (player_init name
                  (get_money player1))
  done;
  init

let bet (player_betting : int) (amount : int) (state : t) (newpb : int) =
  let player = Array.get state.player_array player_betting in
  let total = (get_money_bet player) + amount in
  let hrb = if total > state.high_round_bet
    then total else state.high_round_bet in
  begin
    Array.set state.player_array player_betting
      (player_make (get_name player) ((get_money player) - amount)
         (get_pocket player) (get_folded player)
         ((get_money_bet player) + amount) (get_still_betting player));
    quick_update state (state.pot + amount) hrb state.player_array newpb;
  end

let check (position : int) (state : t) =
  let players = state.player_array in
  let numplayers = Array.length players in
  let next_player = (position + 1) mod numplayers in
  if state.high_round_bet <> 0
  then raise (InvalidBet "Can't check")
  else quick_update state state.pot state.high_round_bet state.player_array
      next_player

let raise_bet (position : int) (amount : int) (state : t)  =
  let players = state.player_array in
  let player = Array.get players position in
  let margin = state.high_round_bet - get_money_bet player in
  let money_left = get_money player - amount - margin in
  let numplayers = Array.length players in
  let next_player = (position + 1) mod numplayers in
  if money_left < 0
  then raise (NotEnoughMoney "Can't raise, not enough money")
  else bet position (margin + amount) state next_player

let fold (position : int) (state : t) =
  let players = state.player_array in
  let player = Array.get players position in
  let next_player = (position + 1) mod (Array.length players) in
  Array.set players position
    (player_make (get_name player) (get_money player)
       (get_pocket player) true (get_money_bet player) false);
  quick_update state state.pot state.high_round_bet state.player_array
    next_player

let all_in (position : int) (state : t) =
  let players = state.player_array in
  let player = Array.get players position in
  let amount = get_money player in
  let money_bet = get_money_bet player in
  let next_player = (position + 1) mod (Array.length players) in
  if amount + money_bet  <= state.high_round_bet then
    bet position amount state next_player
  else raise_bet position (money_bet + amount - state.high_round_bet) state

let call (position : int) (state : t) =
  let players = state.player_array in
  let player = Array.get players position in
  let money_req = state.high_round_bet - get_money_bet player in
  let numplayers = Array.length players in
  let next_player = (position + 1) mod numplayers in
  if state.high_round_bet = 0 then raise (InvalidBet "Can't call") else
  if (get_money player) < money_req
  then begin print_endline "not enough money; going all-in instead";
    all_in position state end
  else bet position money_req state next_player

(** [still_betting_player players] returns a list of the players in [players]
    who are still betting *)
let still_betting_players (players : player list) =
  List.filter (fun x -> get_still_betting x) players

let rec check_if_bets_equal player_list =
  match player_list with
  | h1 :: h2 :: t -> begin
      if get_still_betting h1 && get_still_betting h2 then
        get_money_bet h1 = get_money_bet h2 && check_if_bets_equal (h2 :: t)
      else check_if_bets_equal (h2 :: t) end
  | h :: t -> true
  | [] -> true

let rec check_if_zero_money player_array position acc : player list =
  if position = Array.length player_array then acc
  else let player = Array.get player_array position in
    if get_still_betting player && get_money player = 0 then
      begin Array.set player_array position
          (player_make (get_name player) 0 (get_pocket player)
             (get_folded player) (get_money_bet player) false);
        check_if_zero_money player_array (position + 1)
          (Array.get player_array position :: acc) end
    else check_if_zero_money player_array (position + 1) acc

let rec find_min_bet players acc =
  match players with
  | [] -> (fst acc, (snd acc |> List.rev))
  | h :: t ->
    if get_money_bet h = 0
    then find_min_bet t acc
    else if get_money_bet h < fst acc
    then find_min_bet t (get_money_bet h, [h])
    else if get_money_bet h = fst acc
    then find_min_bet t (fst acc, h :: (snd acc))
    else find_min_bet t acc

let rec create_single_sidepot players min pot players_acc = match players with
  | [] -> (pot, players_acc |> List.rev)
  | h :: t ->
    if get_folded h then
      if get_money_bet h >= min then
        create_single_sidepot t min (min + pot) players_acc
      else create_single_sidepot t min (get_money_bet h + pot) players_acc
    else if get_money_bet h <= 0
    then create_single_sidepot t min pot players_acc
    else create_single_sidepot t min (min + pot) (get_name h :: players_acc)

let rec decrease players money acc =
  match players with
  | h :: t ->
    let amount = if get_money_bet h - money <= 0 then 0
      else get_money_bet h - money in
    let updated_player = player_make (get_name h) (get_money h) (get_pocket h)
        (get_folded h) amount (get_still_betting h) in
    decrease t money (updated_player :: acc)
  | [] -> List.rev acc

let rec check_all_zero_money_bet players : bool =
  match players with
  | h :: t ->
    if get_money_bet h = 0 then check_all_zero_money_bet t else false
  | [] -> true

(** [peek list] returns the head of the list [list] *)
let peek lst =
  match lst with
  | h :: t -> h
  | [] -> failwith "There's nothing to peek"

(** [first_not_zero_money_bet player_list] returns the first player in the
    player list [player_list] who has bet a non-zero amount of money this
    round.  *)
let first_not_zero_money_bet player_list =
  let players_with_not_0_mb = List.filter (fun x -> get_money_bet x <> 0)
      player_list in
  match players_with_not_0_mb with
  | [] -> failwith "There are no players who have bet non-zero money"
  | h :: _ -> h

let rec create_sidepots_aux players acc =
  if check_all_zero_money_bet players then acc
  else
    let still_betting = players |> still_betting_players |> List.rev in
    let ref_player = first_not_zero_money_bet still_betting in
    let min_bets = find_min_bet still_betting
        (get_money_bet ref_player, []) in
    let min_bet = fst min_bets in
    let pot = create_single_sidepot players min_bet 0 [] in
    let decreased_players = decrease players min_bet [] in
    create_sidepots_aux decreased_players (pot :: acc)


(**[create_sidepots] returns a list for state.side_pots
   Makes new side pots that arose from past round, returns list of tuples in
   which each tuple consists of the amount of money in that side pot as well as
   the list of players who are taking part*)
let create_sidepots player_array =
  let players = Array.to_list player_array in
  create_sidepots_aux players []


(** Returns the list of players for whom [money] is not equal to 0
    Also sets [still_betting] to false for all players at [money] = 0*)
let rec get_spillover_pot (sp : (int * string list) list)
    (player_array : player array) : (int * string list) =
  let p_list = snd (peek sp) in
  let still_b_list = Array.to_list player_array |> List.map get_name in
  if List.length p_list > List.length still_b_list
  then (0, still_b_list)
  else (0, [])

(** [reset_money_bet pb pos] resets the field [money_bet] to 0 for each player
    in the player array [pb] *)
let rec reset_money_bet player_array pos =
  match pos with
  | y when y = Array.length player_array -> ()
  | x ->
    let player = Array.get player_array x in
    Array.set player_array x
      (player_make (get_name player) (get_money player)
         (get_pocket player) (get_folded player) 0 (get_still_betting player));
    reset_money_bet player_array (pos + 1)

(** [hrb_update st] returns [st] with [st.high_round_bet] = [0] *)
let hrb_update st = quick_update st st.pot 0 st.player_array st.player_betting

(**[sb_update players acc] updates each player's still_betting field *)
let rec sb_update players acc =
  if acc = Array.length players then ()
  else let player = Array.get players acc in
    if get_still_betting player && get_money player = 0 then begin
      Array.set players acc
        (player_update player (get_money player) (get_money_bet player)
           false false);
      sb_update players (acc + 1)
    end
    else sb_update players (acc + 1)

let create_one_sidepot working_pot tail state players players_w_zero =
  let new_tuple =
    (0, List.filter (fun x -> (List.mem x players_w_zero = false))
       (players |> Array.to_list) |> List.map get_name) in
  let new_state =
    side_pot_update state (new_tuple :: (state.pot, (snd working_pot)) :: tail)
  in reset_money_bet new_state.player_array 0;
  sb_update players 0;
  quick_update new_state 0 new_state.high_round_bet
    new_state.player_array new_state.player_betting |> hrb_update

let check_all_ins state = let players = state.player_array in
  match state.side_pots with
  | [] -> failwith "Error: Sidepots is empty"
  | h :: t -> let sb = still_betting_players (players |> Array.to_list) in
    if (check_if_bets_equal sb) then
      let players_with_zero = check_if_zero_money players 0 [] in
      if (players_with_zero <> [])
      then create_one_sidepot h t state players players_with_zero
      else let _ = reset_money_bet state.player_array 0 in
        sb_update players 0;
        side_pot_update state ((state.pot, (snd h)) :: t) |> hrb_update
    else let sidepots = create_sidepots players in reset_money_bet players 0;
      let new_sp = (sidepots @ t) in
      let new_st = side_pot_update state new_sp in sb_update players 0;
      let spillover = get_spillover_pot new_sp players in
      if (spillover = (0, [])) then quick_update new_st (fst (peek new_sp))
          0 new_st.player_array new_st.player_betting
      else let spill_st = side_pot_update new_st (spillover :: new_sp) in
        quick_update spill_st (fst spillover) 0
          new_st.player_array new_st.player_betting

(** [tuple_compare tup1 tup2] compares the second elements of [tup1] and [tup2]
    Requires: [tup1] and [tup2] are of the type ('a * hand) *)
let tuple_compare tup1 tup2 = hand_compare (snd tup1) (snd tup2)

let get_hands (players : player array) cards  =
  let rec get_hands_aux acc_list acc = let open Array in
    if acc = length players
    then acc_list
    else let player = get players acc in if get_folded player = false
      then let hand = best_hand cards (get_pocket player) in
        get_hands_aux ((player, hand) :: acc_list) (acc + 1)
      else get_hands_aux acc_list (acc + 1)
  in let unsorted = get_hands_aux [] 0 in
  List.sort tuple_compare unsorted |> List.rev

(** [mem_player names player] returns true if a player [player]'s name is in the
    list of strings [names] *)
let mem_player (names : string list) (player : player * hand) : bool =
  List.mem (get_name (fst player)) names

let filter_players (lst : (player * hand) list) (names : string list)
  : (player * hand) list =
  let open List in
  lst |> filter (mem_player names)

let rec winner (lst : (player * hand) list) acc : player list =
  match lst with
  | h1 :: h2 :: t ->
    let compare = hand_compare (snd h1) (snd h2) in
    if compare = 0 then winner (h2 :: t) ((fst h1) :: acc)
    else ((fst h1) :: acc)
  | h :: t -> (fst h) :: acc
  | [] -> acc

(** [add_winnings players amt acc] adds amount [amt] to each player in the
    player array [players] *)
let rec add_winnings players amt acc : player list =
  match players with
  | h :: t ->
    let player = set_player_money h ((get_money h) + amt) in
    add_winnings t amt (player :: acc)
  | [] -> List.rev acc

(** [add_rem pos players amt acc players_acc] adds the remainder of an unevenly
    assigned/divided pot to a random winner's money *)
let rec add_rem position players amt acc players_acc : player list =
  match players with
  | h :: t ->
    if acc = position
    then let player = set_player_money h ((get_money h) + amt) in
      players_acc @ (player :: t)
    else add_rem position t amt (acc + 1) (players_acc @ [h])
  | [] -> failwith "add_rem: Position out of bounds"

(**[divide_sidepot_aux side_pot winners] divides pot of [side_pot] among the
   players in [winners] and returns the list of updated players. *)
let divide_sidepot_aux side_pot winners : player list =
  let len = List.length winners in
  let amt =
    if len = 0 then 0
    else side_pot / len in
  let rem = side_pot - (amt * len) in
  if rem = 0 then add_winnings winners amt []
  else
    let rand = Random.int len in
    add_rem rand winners rem 0 []

(** [find_player name player_array acc] returns the player in the player array
    with name [name] *)
let rec find_player name player_array acc : int =
  if acc = Array.length player_array
  then failwith "find_player: player name not found in array"
  else if get_name (Array.get player_array acc) = name
  then acc
  else find_player name player_array (acc + 1)

(** [update_player_array player_list player_array] updates the player array
    by replacing players in the array with respective players in the player list
    [player_list] *)
let rec update_player_array player_list player_array : unit =
  match player_list with
  | h :: t ->
    let pos = find_player (get_name h) player_array 0 in
    Array.set player_array pos h;
    update_player_array t player_array
  | [] -> ()

let divide_sidepot (players : (player * hand) list)
    (side_pot : int * string list) : player list =
  let amt = fst side_pot in
  let eligible = filter_players players (snd side_pot) in
  let winners = winner eligible [] in
  if amt = 0 then winners
  else divide_sidepot_aux amt winners

let rec distribute state side_pots =
  let cards = (get_flop state) @ (get_turn state) @ (get_river state) in
  let player_array = get_player_array state in
  let players_hands = get_hands player_array cards in
  match side_pots with
  | h :: t -> begin
      let players2 = divide_sidepot players_hands h in
      update_player_array players2 player_array;
      distribute state t
    end
  | [] -> state



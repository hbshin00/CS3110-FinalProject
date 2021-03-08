open Cards
open State


(** [deck_stripper pocket_and_table deck acc] checks if each card from card list
    [deck] is in a list of cards [pocket_and_table] and and if not, adds it to
    the accumulator [acc] and finally returns the list [acc].
    This is used for creating a deck without instances of already dealt cards.*)
let rec deck_stripper (pocket_and_table : card list) (deck : card list) 
    acc : card list =
  match deck with
  | c :: t -> begin if List.mem c pocket_and_table
      then deck_stripper pocket_and_table t acc
      else deck_stripper pocket_and_table t (c :: acc) end
  | [] -> acc

(** [top_cards deck num_cards] takes in the number of cards [num_cards] required
    to complete a full river on the table, and returns that many cards from the
    top of the card list [deck]. *)
let top_cards (deck : card list) num_cards =
  match deck with
  | c1 :: c2 :: c3 :: c4 :: c5 :: _ -> begin
      match num_cards with
      | 5 -> c1 :: c2 :: c3 :: c4 :: c5 :: [] (* complete f, t, & r *)
      | 2 -> c1 :: c2 :: [] (* complete t & r *)
      | 1 -> c1 :: [] (* complete r *)
      | 0 -> []
      | _ -> failwith "wrong number of cards to complete the river"
    end
  | _ -> failwith "deck shorter than 5 cards"

(** [dummy_maker num_players acc] returns a list of [num_players] players with
    no purpose other than to hold cards for comparison with later. *)
let rec dummy_maker num_players (acc : player list) =
  match num_players with
  | a when a >= 1 -> dummy_maker (num_players - 1) 
                       (player_init ("dummy"^ string_of_int a) 1000 :: acc)
  | b when b < 1 -> acc
  | _ -> failwith "TODO: some wrong number of players exception?"

(** [monte_carlo_aux pocket table acc win_acc tie_acc num_players]
    [pocket]: the card list of the player's 2 cards in hand
    [table]: the card list of cards visible to the player on the table
    [acc]: the total number of iterations of monte_carlo_aux,
            initialialed to 0
    [win_acc]: the number of wins a pocket has in the simulation,
            initialialed to 0
    [tie_acc]: the number of tie wins a pocket has in the simulation,
            initialialed to 0
    [num_players]: the number of players on the table being tested against
    returns: (acc, win_acc, tie_acc) *)
let rec monte_carlo_aux pocket table acc win_acc tie_acc num =
  if acc = 500 then (acc, win_acc, tie_acc)
  else let table_length = List.length table in
    let dummy_players = dummy_maker num [] |> Array.of_list in
    let shuf_strip_deck = shuffle (deck_stripper (pocket @ table) std_deck []) 
    in let rem_deck = deal shuf_strip_deck dummy_players 0 in
    let full_players = Array.append 
        [|player_make "self" 1000 pocket false 0 true |] dummy_players in
    win_checker full_players pocket table rem_deck (5 - table_length) 
      acc win_acc tie_acc num

(**[win_checker full_players pocket table rem_deck num_left acc win_acc
   tie_acc numplayers]
   [full_players]: the array of players (player and the dummy list)
   [pocket]: the card list of the player's 2 cards in hand
   [table]: the card list of cards visible to the player on the table
   [rem_deck]: the cards left in the deck after the top cards are dealt out
   [num_left]: the number of cards required from the deck to complete a river
   [acc]: the total number of iterations of monte_carlo_aux,
            initialialed to 0
   [win_acc]: the number of wins a pocket has in the simulation,
            initialialed to 0
   [tie_acc]: the number of tie wins a pocket has in the simulation,
            initialialed to 0
   [num_players]: the number of players on the table being tested against
   returns: (acc, win_acc, tie_acc)*)
and win_checker full_players pocket table rem_deck num_left acc win_acc tie_acc 
    num_players  =
  let rem_table_cards = top_cards rem_deck num_left in
  let player_hands = get_hands full_players (table @ rem_table_cards) in
  let filtered_lst = filter_players player_hands 
      (List.map get_name (Array.to_list full_players)) in
  let winners = winner filtered_lst [] |> List.map get_name in
  let yes_win = List.mem "self" winners in
  if yes_win && List.length winners = 1 then
    monte_carlo_aux pocket table (acc + 1) (win_acc + 1) tie_acc  num_players
  else if (yes_win) then monte_carlo_aux pocket table (acc + 1) (win_acc + 1) 
      (tie_acc + 1)  num_players
  else monte_carlo_aux pocket table (acc + 1) win_acc tie_acc num_players

let monte_carlo pocket table num_players =
  monte_carlo_aux pocket table 0 0 0 num_players

(**[odds str mc_tuple] returns a float (percentage) of wins, ties, or both as
   specified by [str]. The str "both_w" returns the percentage of both wins and
   ties with ties given 0.5 weight. Fails if [str] is not a valid specifier.*)
let odds str mc_tuple : float =
  match mc_tuple with
  | (all, wins, ties) ->
    let allflt = float_of_int all in
    let winflt = float_of_int wins in
    let tieflt = float_of_int ties in
    if str = "win" then winflt /. allflt
    else if str = "tie" then tieflt /. allflt
    else if str = "both" then (winflt +. tieflt) /. allflt
    else if str = "both_w" then (winflt +. (tieflt /. 2.)) /. allflt
    else failwith "odds: not a valid string specifier"

let pocket_odds trait odds =
  if trait = "cons" then
    if odds > 0.8 then (1, 4) (**Raise_bet *)
    else if odds > 0.7 then (1, 6) (**Raise_bet*)
    else if odds > 0.35 then (1, 8) (**Raise_bet *)
    else if odds > 0.1 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else if trait = "aggr" then
    if odds > 0.7 then (1, 4) (**Raise_bet *)
    else if odds > 0.5 then (1, 5) (**Raise_bet*)
    else if odds > 0.3 then (1, 6) (**Raise_bet *)
    else if odds > 0.1 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else failwith "pocket_odds: invalid trait"

let flop_odds trait odds =
  if trait = "cons" then
    if odds > 0.65 then (1, 4) (*All_in*)
    else if odds > 0.5 then (1, 6) (**Raise_bet*)
    else if odds > 0.3 then (1, 8) (**Raise_bet *)
    else if odds > 0.11 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else if trait = "aggr" then
    if odds > 0.6 then (1, 4) (**Raise_bet *)
    else if odds > 0.4 then (1, 5) (**Raise_bet*)
    else if odds > 0.25 then (1, 6) (**Raise_bet *)
    else if odds > 0.1 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else failwith "flop_odds: invalid trait"

let turn_odds trait odds =
  if trait = "cons" then
    if odds > 0.8 then (1, 4) (*All_in*)
    else if odds > 0.6 then (1, 6) (**Raise_bet*)
    else if odds > 0.4 then (1, 8) (**Raise_bet *)
    else if odds > 0.12 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else if trait = "aggr" then
    if odds > 0.7 then (1, 4) (**Raise_bet *)
    else if odds > 0.5 then (1, 5) (**Raise_bet*)
    else if odds > 0.3 then (1, 6) (**Raise_bet *)
    else if odds > 0.1 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else failwith "flop_odds: invalid trait"

let river_odds trait odds =
  if trait = "cons" then
    if odds > 0.8 then (1, 4) (*All_in*)
    else if odds > 0.5 then (1, 6) (**Raise_bet*)
    else if odds > 0.35 then (1, 8) (**Raise_bet *)
    else if odds > 0.13 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else if trait = "aggr" then
    if odds > 0.7 then (1, 4) (**Raise_bet *)
    else if odds > 0.4 then (1, 5) (**Raise_bet*)
    else if odds > 0.3 then (1, 6) (**Raise_bet *)
    else if odds > 0.1 then (2, -1) (**Call *)
    else if odds > 0.05 then (3, -1) (**Check *)
    else (0, -1) (**Fold *)
  else failwith "flop_odds: invalid trait"





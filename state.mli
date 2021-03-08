open Cards

(** Raised when a player tries to raise more money than is available *)
exception NotEnoughMoney of string

(** Raised when a player makes an invalid bet (e.g., if a player tries to
    [check] when money has already been bet this round) *)
exception InvalidBet of string

(** [t] is the type of game states. *)
type t

(** [update_state dealer fp tn rvr cp hrb mrb pa pb sps] returns a new state
    with [dealer] = [dealer], [flop] = [fp] , [turn] = [tn], [rvr] = [river],
    [pot] = [cp], [high_round_bet] = [hrb],
    [player_array] = [pa], [player_betting] = [pb], [side_pots] = [sps] *)
val update_state : int -> Cards.card list -> Cards.card list ->
  Cards.card list -> int -> int -> Cards.player array -> int ->
  (int * string list) list -> t

(** [quick_update t cp hrb pa pb] returns state [t] with pot [cp],
    high_round_best [hrb], player array [pa], and player betting [pb] *)
val quick_update : t -> int -> int -> Cards.player array -> int -> t

(** [side_pot_update state sp] takes in state [state] and side_pot list [sp] and
    returns the new state with updated sidepots*)
val side_pot_update : t -> (int * string list) list -> t

(** [init_state deck pa] creates a new state with the dealer assigned to array
    position 0, and designates the flop, turn, and river in the deck. It also
    assigns the player array [pa] to field [player_array]. [deck] can be
    shuffled or unshuffled. *)
val init_state : card list -> player array -> t

(** [reset_players player_array] takes in the player array, and re-initializes
    all players with their new money for the next round *)
val reset_players : Cards.player array -> unit

(** [bet pos amt t] takes the amount bet [amt] from the money of the player
    making the bet, and adds it to the pot
    [position] is the index of the betting player in the player_array
    [amount] is the amount of money being bet >= 0*)
val bet : int -> int -> t -> int -> t

(** [check pos t] returns the updated state after the player in position [pos]
    in the player array of state [t] checks; bets nothing, and updates
    [player_betting]
    Requires:
       [pos] is the index of the player currently betting in the player_array
       of state [t]
    Fails if highest_round_bet greater than 0 *)
val check : int -> t -> t

(** [raise_bet pos amt t] updates state [t] after the player in position [pos]
    in the player array raises by amount [amt], and returns the updated state.
    The amount [amt] raised is the amount by which the player wants to increase
    the highest round bet.
    Requires:
       [pos] is the index of the betting player in the player_array
       [amt] is the amount of money being raised; must be an int > 0
    Raises: NotEnoughMoney exception if [amt] > () *)
val raise_bet : int -> int -> t -> t

(** [fold pos t] returns the updated state after the player in position [pos] in
    the player arrary in state [t] folds.
    Requires:
       [position] is the index of the betting player in the player_array
       [amount] is the amount of money being bet >= 0*)
val fold : int -> t -> t

(** [all_in pos t] calls [bet] with [amt] = [get_money h], where [h] is the
    player in position [pos] in the player array of state [t]
    Requires:
       [pos] is the index of the betting player in the player_array
       [t] is the current state *)
val all_in : int -> t -> t

(** [call pos t] calls [bet] with [amt] = highest_round_bet - money_bet
    (matches highest bet given what the player has already bet)
    Requires:
       [pos] is the index of the betting player in the player_array
       [t] is the current state*)
val call : int -> t -> t

(** [let check_if_bets_equal pa] returns true if all players still betting have
    bet the same amount this round *)
val check_if_bets_equal : Cards.player list -> bool

(** [check_if_zero_money pa pos acc] return an list of players
    who are still betting and have 0 money
    [acc] is a list of players who have 0 money*)
val check_if_zero_money : Cards.player array -> int -> Cards.player list ->
  Cards.player list

(** [find_min_bet players acc] returns the minimum bet among the players still
    betting *)
val find_min_bet : Cards.player list -> int * Cards.player list ->
  int * Cards.player list

(** [create_single_sidepot] returns a pot, which is a tuple (amt, players) such
    that [amt] is the total amount of money in the pot and [players] is the list
    of players who are taking part in this pot*)
val create_single_sidepot : Cards.player list ->
  int -> int -> string list -> int * string list

(** [decrease] returns the same list of players but with their money_bet fields
    reduced by the input [money].
    This is meant as a helper for creating sidepots.*)
val decrease : Cards.player list -> int -> Cards.player list ->
  Cards.player list

(**[check_all_zero_money_bet] returns true if all players have money_bet = 0,
   false otherwise *)
val check_all_zero_money_bet : Cards.player list -> bool

(** [create_sidepot_aux] returns a list of pots where each pot is a tuple
    (amt, players) such that [amt] is the total amount of money in that pot and
    [players] is the list of players who are taking part in that pot.  *)
val create_sidepots_aux : Cards.player list ->
  (int * string list) list -> (int * string list) list

(** [get_spillover_pot player_array position acc] returns the list of players
    for whom [money] is not equal to 0. Also sets [still_betting] to false for
    all players at [money] = 0 *)
val get_spillover_pot : (int * string list) list -> Cards.player array ->
  int * string list (* change to return an int * string list *)

(** [reset_money_bet] takes in [player_array] and resets every player's   
    money_bet to 0, starting from index [pos] *)
val reset_money_bet : Cards.player array -> int -> unit

(** [check_all_ins state] takes in the [state] and checks if anyone has gone
    all-in, and if so, creates side pots accordingly, and returns the state.*)
val check_all_ins : t -> t

(** [player_array_make_cpus number_cpus player1] adds [number_cpus] players to
    a player array initialized on player [player1] *)
val player_array_make_cpus : int -> Cards.player -> Cards.player array

(** [get_dealer t] returns the position of the dealer for state [t] *)
val get_dealer : t -> int

(** [get_flop t] returns the flop of the state [t] as a card list *)
val get_flop : t  -> card list

(** [get_turn t] returns the turn of the state [t] as a card list *)
val get_turn : t -> card list

(** [get_river t] returns the river of the state [t] as a card list *)
val get_river : t -> card list

(** [get_pot t] returns the pot of state [t] *)
val get_pot : t  -> int

(** [get_hrb t] returns the highest_round_bet of state [t] *)
val get_hrb : t -> int

(** [get_player_array t] returns the player array of state [t] *)
val get_player_array : t -> Cards.player array

(** [get_player_betting t] returns the position of the player betting next *)
val get_player_betting : t -> int

(** [get_side_pots t] returns the side pots of state [t] *)
val get_side_pots : t -> (int * string list) list

(** [distribute state side_pots] distributes money from each of the side pots in
    [side_pots] by determining the winners of each, and assigning money to
    winners. Returns the updated state.  *)
val distribute : t -> (int * string list) list -> t

(** [get_hands players cards] returns the ordered list of tuples of type
    (player * hand), where each tuple corresponds to a player and their
    respective best possible hand that round. *)
val get_hands : Cards.player array -> Cards.card list -> 
  (Cards.player * Cards.hand) list

(** [filter_players lst names] returns the (player * hand) list without the
    players who are no longer betting. *)
val filter_players : (player * hand) list -> string list -> 
  (player * hand) list

(** [winner lst acc] returns the list of players who have won the round. *)
val winner : (Cards.player * Cards.hand) list -> Cards.player list -> 
  Cards.player list



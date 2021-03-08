module type CardsSig = sig
  exception NotEnoughCards of string
  type card
  type hand
  type player
  val std_deck : card list
  val suitMaker : card list -> string -> int -> card list
  val shuffle : card list -> card list
  val deal : card list -> player array -> int -> card list
  val high : card list -> hand
  val pair : card list -> hand
  val is_twopair : card list  -> bool
  val is_trips : card list -> bool
  val is_straight : card list -> bool
  val is_flush : card list -> bool 
  val is_fullhouse : card list -> bool
  val is_four : card list -> bool
  val is_straightflush : card list -> bool
  val royalflush : card list -> hand
  val hand_of_cards : card list -> hand
  val best_hand : card list -> card list -> hand
  val get_name : player -> string
  val get_money : player -> int
  val get_money_bet : player -> int
  val get_pocket : player -> card list
  val get_folded : player -> bool 
  val get_still_betting : player -> bool
  val player_make : string -> int -> card list -> bool -> int -> bool -> player
  val player_init : string -> int -> player
  val set_player_money : player -> int -> player
  val pp_hand : hand -> string 
  val get_suit : card -> string
  val get_number : card -> int 
  val card_make : string -> int -> card 
  val high_make : int list -> hand
  val pair_make : int -> int list -> hand
  val twopairs_make : int -> int -> int -> hand
  val trips_make : int -> int -> int -> hand
  val straight_make : int -> hand 
  val flush_make : int list -> hand 
  val four_make : int -> int -> hand
  val fullhouse_make : int -> int -> hand
  val straightflush_make : int -> hand
  val royalflush_make : hand
  val player_update : player -> int -> int -> bool -> bool -> player
  val hand_compare : hand -> hand -> int
end

module CardsCheck : CardsSig = Cards

module type StateSig = sig
  exception NotEnoughMoney of string
  exception InvalidBet of string 
  type t 
  val update_state : int -> Cards.card list -> Cards.card list ->
    Cards.card list -> int -> int -> Cards.player array -> int ->
    (int * string list) list -> t
  val quick_update : t -> int -> int -> Cards.player array -> int -> t
  val side_pot_update : t -> (int * string list) list -> t
  val init_state : Cards.card list -> Cards.player array -> t
  val reset_players : Cards.player array -> unit
  val bet : int -> int -> t -> int -> t
  val check : int -> t -> t 
  val raise_bet : int -> int -> t -> t
  val fold : int -> t -> t
  val all_in : int -> t -> t
  val call : int -> t -> t 
  val check_if_bets_equal : Cards.player list -> bool
  val check_if_zero_money : Cards.player array -> int -> Cards.player list -> 
    Cards.player list
  val find_min_bet : Cards.player list -> int * Cards.player list -> 
    int * Cards.player list
  val create_single_sidepot : Cards.player list ->
    int -> int -> string list -> int * string list
  val decrease : Cards.player list -> int -> Cards.player list -> 
    Cards.player list
  val check_all_zero_money_bet : Cards.player list -> bool
  val create_sidepots_aux : Cards.player list ->
    (int * string list) list -> (int * string list) list
  val get_spillover_pot : (int * string list) list -> Cards.player array ->
    int * string list
  val reset_money_bet : Cards.player array -> int -> unit
  val check_all_ins : t -> t
  val player_array_make_cpus : int -> Cards.player -> Cards.player array
  val get_dealer : t -> int
  val get_flop : t  -> Cards.card list
  val get_turn : t -> Cards.card list
  val get_river : t -> Cards.card list
  val get_pot : t  -> int
  val get_hrb : t -> int
  val get_player_array : t -> Cards.player array
  val get_player_betting : t -> int
  val get_side_pots : t -> (int * string list) list
end

module StateCheck : StateSig = State

module type AISig = sig
  val monte_carlo : Cards.card list -> Cards.card list -> int -> int * int * int
  val odds : string -> int * int * int -> float
  val pocket_odds : string -> float -> int * int
  val flop_odds : string -> float -> int * int
  val turn_odds : string -> float -> int * int
  val river_odds : string -> float -> int * int
end

module AICheck : AISig = Ai 

module type AuthorSig = sig
  val hours_worked : int
end

module AuthorCheck : AuthorSig = Author


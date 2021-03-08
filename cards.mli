(** Raised when there aren't enough cards in the deck for the number of players 
    when calling [deal] *)
exception NotEnoughCards of string

(** Type of a card; has fields [suit] and [number]
    Requires: 
      [suit] is a string representing the suit of the card; must be one of 
        the following: ["Hearts"], ["Diamonds"], ["Clubs"], ["Spades"]
      [number] must be in the range 2..14, where 11, 12, 13, and 14 
      represent Jack, Queen, King, and Ace, respectively. *)
type card

(** Type of a hand; has 10 type constructors corresponding to the 10 possible 
    hands:
    | High of int list
    | Pair of int * int list
    | TwoPairs of int * int * int desc order: high pair, low pair, kicker 
    | Trips of int * int * int
    | Straight of int (* int is highest value in straight*)
    | Flush of int list (* all values in flush*)
    | FullHouse of int * int (*fst int represents trip, snd int is pair*)
    | Four of int * int (*fst is four of a kind, snd is kick *)
    | StraightFlush of int
    | RoyalFlush
*)
type hand

(** Type of a player; has fields [name], [money], and [pocket]
    Requires: 
      [name] is any non-empty string
      [money] >= 0
      [pocket] list of 2 cards in pocket *)
type player

(** [shuffle deck] returns a shuffled deck of  
    Requires:
    [deck] is list of cards *)
val shuffle : card list -> card list

(** An unshuffled, standard deck of 52 cards, sorted in the following order: 
    Hearts, Diamonds, Clubs, Spades; each individual suit is sorted: 2..King, 
    Ace *)
val std_deck : card list

(** [deal deck player_array acc] deals two cards to each player (dealing from 
    the front of [deck] first), and returns the deck with the remaining cards.
    Requires:  
        [deck] is a card list to be dealt (must have length 52 when [deal] is 
        first called)
        [player_array] is a player array of length 2..8 
        [acc] is the index of the player in the array whose cards are being 
        dealt; is an int between 0 and the length of [player_array]
    Returns: the undealt cards as a card list
    Raises: NotEnoughCards exn when there aren't enough cards in the deck for 
    the number of players. *)
val deal : card list -> player array -> int -> card list

(** [suitMaker lst suit number] creates 13 cards with suit [suit], starting 
    from 2, and finishing with 13 (King), and then 14 (Ace); returns a card 
    list in this order. 
    Requires:
      [lst] is a card list that is empty when [suitMaker] is first called
      [suit] is "Hearts", "Diamonds", "Clubs", or "Spades"
      [number] is an int in 2..14; must be 2 when [suitMaker] is first called *)
val suitMaker : card list -> string -> int -> card list

(** [high cards] returns [High lst], the hand to which [cards] corresponds 
    Requires: [cards] is a list of cards corresponding to a high card hand with 
      the same cards as [lst] *)
val high : card list -> hand

(** [is_pair cards acc] returns true if [cards] is a list of cards corresponding
    to a pair, and returns false otherwise. *)
val pair : card list -> hand

(** [is_twopair cards] returns true if the card list [cards] constitutes a 
    TwoPair and false otherwise *)
val is_twopair : card list  -> bool

(** [is_trips cards] returns true if the card list [cards] constitutes a Trips 
    (three-of-a-kind) and false otherwise *)
val is_trips : card list -> bool

(** [is_straight cards] takes in a sorted card list [cards] (descending order)    
    and returns true if the cards constitute a straight, false otherwise. *)
val is_straight : card list -> bool

(** [is_flush cards] takes in a sorted card list [cards] (descending order) and 
    returns true if the cards constitute a flush. *)
val is_flush : card list -> bool 

(** [is_fullhouse cards] returns true if the card list [cards] constitutes a    
    FullHouse and false otherwise *)
val is_fullhouse : card list -> bool

(** [is_four cards] returns true if the card list [cards] constitutes a Four 
    (four-of-a-kind) and false otherwise *)
val is_four : card list -> bool

(** [is_straight_flush cards] takes in a sorted card list [cards] 
    (descending order) and returns true if the cards constitute a straight 
    flush. *)
val is_straightflush : card list -> bool

(** [royalflush cards] returns the RoyalFlush hand corresponding to cards 
    if the cards constitutes a royal flush, and if not, it raises exception 
    WrongHand. *)
val royalflush : card list -> hand

(** [hand_of_cards cards] returns the hand corresponding to the cards in 
    [cards], where [cards] is a list of 5 cards representing the 
    best 5 cards in a player's hand. *)
val hand_of_cards : card list -> hand

(** [best_hand table pocket] returns the best hand made from the card list     
    [pocket] and the card list [table]    
    Requires: the combined length of [table] and [pocket] should be 7 *)
val best_hand : card list -> card list -> hand

(** [get_name player] returns the name of [player] *) 
val get_name : player -> string

(** [get_money player] returns the money left for [player] *) 
val get_money : player -> int

(** [get_money_bet player] returns the amount of money bet by [player] in this
    round. *) 
val get_money_bet : player -> int

(** [get_pocket player] returns the pocket (cards in hand) of [player] *) 
val get_pocket : player -> card list

(** [get_folded player] returns [true] if [player] has folded, and [false] 
    otherwise *) 
val get_folded : player -> bool 

(** [get_folded player] returns [true] if [player] is still betting, and 
    [false] otherwise (i.e., it returns [true] if [player] did not go all-in 
    this round) *) 
val get_still_betting : player -> bool

(** [player_make name money pocket folded bet still] makes a record for a player 
    with field values [name] = [name], [money] = [money], [pocket] = [pocket], 
    [folded] = [folded], [money_bet] = [bet], and [still_betting] = [still] *)
val player_make : string -> int -> card list -> bool -> int -> bool -> player

(** [player_make name money] initializes a [player] record with field values 
    [name] = [name], [money] = [money], [pocket] = [[]], [folded] = [false], 
    [money_bet] = [0], and [still_betting] = [true] *)
val player_init : string -> int -> player

(** [set_player_money player money] returns [player] with money [money]. *)
val set_player_money : player -> int -> player

(** [pp_hand hand] pretty prints a hand *)
val pp_hand : hand -> string

(** [get_suit card] returns the suit of [card] *) 
val get_suit : card -> string

(** [get_number card] returns the number of [card] *) 
val get_number : card -> int 

(** [card_make suit number] creates a [card] record with [suit] = [suit] and 
    [number] = [number] *)
val card_make : string -> int -> card 

(** [high_make lst] makes a hand of type constructor High using the sorted card
    list. 
    Requires: [lst] is a list of cards corresponding to a High card hand.*)
val high_make : int list -> hand

(** [pair_make i1 lst] makes a hand of type constructor Pair. 
    Requires: [lst] is a list of cards corresponding to a Pair of cards with
    number [i1]  *)
val pair_make : int -> int list -> hand

(** [pair_make i1 i2 i3] makes a hand of type constructor TwoPair of [i1] and 
    [i2], where [i1] and [i2] are the numbers corresponding to the two pairs, 
    and [i3] is the number of the last card in the hand. 
    Requires: i1 is an int that is greater than i2, and i2 is greater than i3 *)
val twopairs_make : int -> int -> int -> hand

(** [pair_make i1 i2 i3] makes a hand of type constructor Trips of [i1] where 
    [i1] is the number corresponding to the three-of-a-kind, and [i2] and [i3] 
    are the numbers of the lasts card in the hand, in descending order. 
    Requires: i2 is greater than i3 *)
val trips_make : int -> int -> int -> hand

(** [straight_make int] makes a hand of type constructor Straight with the 
    highest card of number [int]. *)
val straight_make : int -> hand 

(** [flush_make lst] makes a hand of type constructor Flush with the list of 
    cards [lst], where [lst] is sorted by number of the card. *)
val flush_make : int list -> hand 

(** [four_make i1 i2] makes a hand of type constructor Four (four-of-a-kind), 
    where [i1] is the number of the card of which there are four-of-a-kind, and 
    [i2] is the number of the last card in the hand of five cards. *)
val four_make : int -> int -> hand

(** [fullhouse_make i1 i2] makes a hand of type constructor FullHouse of [i1] 
    and [i2], where [i1] is the number of the three-of-a-kind, and [i2] is the 
    numbers corresponding to the pair. *)
val fullhouse_make : int -> int -> hand

(** [straightflush_make i1] makes a hand of type StraightFlush with the highest
    card number [i1] *)
val straightflush_make : int -> hand

(** [royalflush_make] makes a hand of type RoyalFlush. *)
val royalflush_make : hand

(** [player_update player money money_bet folded still] updates a player with 
    the corresponding fields.  *)
val player_update : player -> int -> int -> bool -> bool -> player

(** [hand_compare hand1 hand2] compares [hand1] and [hand2], which have the 
    same hand type constructor (e.g., they're both straights), and returns:
    1 if [hand1] is better than [hand2]
    -1 if [hand2] is better than [hand1]
    0 otherwise. *)
val hand_compare : hand -> hand -> int
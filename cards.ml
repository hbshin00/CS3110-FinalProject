exception NotEnoughCards of string
exception WrongHand of string

type card = {
  suit : string;
  number : int;
}

(* [card] getters and setters *)
let card_make suit number = {suit = suit; number = number}
let get_suit card = card.suit 
let get_number card = card.number

type hand = 
  | High of int list
  | Pair of int * int list
  | TwoPairs of int * int * int (*descending order: high pair, low pair, kick*)
  | Trips of int * int * int
  | Straight of int (* int is highest value in straight*)
  | Flush of int list (* all values in flush*)
  | FullHouse of int * int (*fst int represents trip, snd int represents pair*)
  | Four of int * int (*fst is four of a kind, snd is kick *)
  | StraightFlush of int
  | RoyalFlush

(** [pp_card c] pretty-prints card [c]. *)
let pp_card (c : card) = 
  "\"" ^ ((c.number) |> string_of_int) ^ " " ^ (c.suit) ^ "\""

let pp_hand = function
  | High [a; b; c; d; e] ->
    "High: " ^ string_of_int a ^ ", " ^ string_of_int b ^ ", " ^ string_of_int c
    ^ ", " ^ string_of_int d ^ ", " ^ string_of_int e
  | Pair (int, [int1; int2; int3]) ->
    "Pair of " ^ string_of_int int ^ ", " ^ string_of_int int1 ^
    ", " ^ string_of_int int2 ^ ", " ^ string_of_int int3
  | TwoPairs (int1, int2, int3) ->
    "Two Pairs: pair of " ^ string_of_int int1 ^ ", pair of " ^
    string_of_int int2 ^ ", " ^ string_of_int int3
  | Trips (int, int1, int2) ->
    "Three-of-a-kind of " ^ string_of_int int ^
    ", " ^ string_of_int int1 ^ ", " ^ string_of_int int2
  | Straight int -> "Straight: highest value of " ^ string_of_int int
  | Flush [a; b; c; d; e] ->
    "Flush: " ^ string_of_int a ^ ", " ^ string_of_int b ^ ", " 
    ^ string_of_int c ^ ", " ^ string_of_int d ^ ", " ^ string_of_int e
  | FullHouse (int1, int2) -> "Full House: triple of " ^ string_of_int int1 ^
                              ", pair of " ^ string_of_int int2
  | Four (int1, int2) ->
    "Four-of-a-kind of " ^ string_of_int int1 ^ ", " ^ string_of_int int2
  | StraightFlush int -> "Straight Flush: highest value of " ^ string_of_int int
  | RoyalFlush -> "Royal Flush"
  | _ -> failwith "can't print this, as it is not a hand"

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

let combinations = [ 
  [0; 1; 2; 3; 4]; [0; 1; 2; 3; 5]; [0; 1; 2; 3; 6]; 
  [0; 1; 2; 4; 5]; [0; 1; 2; 4; 6]; [0; 1; 2; 5; 6];
  [0; 1; 3; 4; 5]; [0; 1; 3; 4; 6]; [0; 1; 3; 5; 6];
  [0; 1; 4; 5; 6]; [0; 2; 3; 4; 5]; [0; 2; 3; 4; 6];
  [0; 2; 3; 5; 6]; [0; 2; 4; 5; 6]; [0; 3; 4; 5; 6];
  [1; 2; 3; 4; 5]; [1; 2; 3; 4; 6]; [1; 2; 3; 5; 6];
  [1; 2; 4; 5; 6]; [1; 3; 4; 5; 6]; [2; 3; 4; 5; 6]
]

(* [hand] getters and setters *)
let high_make lst = High lst
let pair_make int lst = Pair (int, lst) 
let twopairs_make int1 int2 int3 = TwoPairs (int1, int2, int3)
let trips_make int1 int2 int3 = Trips (int1, int2, int3)
let straight_make int = Straight int 
let flush_make lst = Flush lst
let four_make int1 int2 = Four (int1, int2) 
let fullhouse_make int1 int2 = FullHouse (int1, int2)
let straightflush_make int = StraightFlush int
let royalflush_make = RoyalFlush

type player = 
  {
    name : string; 
    money : int; 
    pocket : card list;
    folded : bool; 
    money_bet : int;
    still_betting : bool;
  } 

(* [player] getters and setters *)
let player_make name money pocket folded money_bet still_betting : player = 
  {name = name; money = money; pocket = pocket; folded = folded; 
   money_bet = money_bet; still_betting = still_betting}
let player_init name money = player_make name money [] false 0 true 
let player_update player money money_bet folded still = 
  {player with money = money; money_bet = money_bet; 
               still_betting = still; folded = folded}
let player_new_pocket player pocket = {player with pocket = pocket}
let set_player_money player int = {player with money = int}
let get_name player = player.name
let get_money player = player.money
let get_money_bet player = player.money_bet
let get_pocket player = player.pocket
let get_folded player = player.folded
let get_still_betting player = player.still_betting

let rec suitMaker (lst : card list) (suit : string) (acc : int) = 
  if acc = 15 then lst
  else let temp_card = {suit = suit; number = acc} in 
    suitMaker (temp_card :: lst) suit (acc + 1)

(** Creates the 4 suits for the unshuffled deck. *)
let hearts = suitMaker [] "Hearts" 2
let diamonds = suitMaker [] "Diamonds" 2
let clubs = suitMaker [] "Clubs" 2
let spades = suitMaker [] "Spades" 2

let std_deck = hearts @ diamonds @ clubs @ spades

(** [shuffle_helper] takes in a list of tuples (of type (int * card)) and adds 
    the card from each tuple to a card list in the order of the tuple list
    Requires:
      [acc_lst] is empty when [shuffle_helper] first called
      the tuple list being matched has type (int * card) *)
let rec shuffle_helper acc_lst = function
  | [] -> acc_lst
  | h :: t -> shuffle_helper (snd h :: acc_lst) t

let shuffle deck =
  Random.self_init ();
  List.map (fun c -> (Random.int 5000, c)) deck 
  |> List.sort compare 
  |> shuffle_helper []

(** [deal_helper] goes through [player_array] and assigns the card list [pocket] 
    to the player in position [acc] in the array.
    Requires: 
    [pocket] is a card list of length 2
    [player_array] is a player_array of length 2..23 
    [acc] is the index of the player in the array whose cards are being dealt; 
      is an int between 0 and the length of player_array *)
let deal_helper pocket player_array acc = let open Array in
  let player = get player_array acc in if player.money = 0 then () else
    set player_array acc {(get player_array acc) with pocket = pocket}

let rec deal deck player_array acc =
  if acc =  Array.length player_array then deck
  else
    match deck with
    | h1 :: h2 :: t -> deal_helper (h1 :: h2 :: []) player_array acc;
      deal t player_array (acc + 1)
    | [] -> raise (NotEnoughCards "Too many players")
    | h :: t -> raise (NotEnoughCards "Too many players") 

(** [get_card cards int acc] returns the card at position [int] in the card list 
    [cards] *)
let rec get_card cards int acc = 
  match cards with 
  | h :: t -> 
    if acc = int then h else get_card t int (acc + 1)
  | [] -> failwith "get_card: This index is out of bounds given these cards" 

(**[possible_hands combinations] takes in the pre-existing value [combinations],   
   some 7-card card list [cards], and an empty [acc].    
   It generates all the possible 5-card combinations of [cards], and returns    
   them as a (card list) list.   Fails if the input [combinations] is not the 
   pre-existing value.*)
let rec possible_hands combinations (cards : card list) 
    (acc : (card list) list) : (card list) list = 
  match combinations with 
  | [a; b; c; d; e] :: t ->
    let card_a = get_card cards a 0 in 
    let card_b = get_card cards b 0 in 
    let card_c = get_card cards c 0 in 
    let card_d = get_card cards d 0 in 
    let card_e = get_card cards e 0 in 
    let five = card_a :: card_b :: card_c :: card_d :: card_e :: [] in 
    possible_hands t cards (five :: acc)
  | [] -> acc
  | _ -> 
    failwith "possible_hands: combinations shouldn't be 
      anything other than combinations"

(** [card_compare card1 card2] takes in two cards and returns the larger   
    of the two, returns 1 if card1 is the larger of the two, returns -1 if    
    card2 is the larger of the two, and returns 0 if the two cards have equal    
    numerical value. When used with List.sort, puts the cards in descending 
    order. *)
let card_compare card1 card2 : int = 
  let num1 = card1.number in 
  let num2 = card2.number in 
  if num1 < num2 then 1 
  else if num1 = num2 then 0 
  else if num1 > num2 then -1
  else failwith "card_compare: these two cards cannot be compared"

(** [num_true bools acc] returns the number of times [true] occurs in the     
    list of bools [bools]. *)
let rec num_true bools acc : int = 
  match bools with
  | h :: t ->
    if h then num_true t (acc + 1) 
    else num_true t acc 
  | [] -> acc

(** [strip_cards cards acc] strips all of the cards in [cards] of their suits,
    and returns a list of the cards' numbers, in the same order.*)
let rec strip_cards cards acc : int list = 
  match cards with 
  | h :: t -> 
    let num = h.number in 
    strip_cards t (num :: acc) 
  | [] -> List.rev acc

let high (cards : card list) : hand = 
  let sorted = List.sort card_compare cards in 
  let stripped = strip_cards sorted [] in
  High stripped

(** [is_pair_aux card cards acc] returns [true] if the card [card] has a     
    matching card (numerically) in the card list [cards] *)
let rec is_pair_aux card cards acc : bool = 
  let num = card.number in
  match cards with 
  | h :: t -> 
    let numh = h.number in 
    if num = numh then is_pair_aux card t (h :: acc)
    else is_pair_aux card t acc 
  | [] -> 
    if List.length acc = 2 then true else false

let rec is_pair (cards : card list) (acc : bool list) : bool =
  match cards with 
  | h :: t -> 
    let pair = is_pair_aux h cards [] in 
    is_pair t (pair :: acc)
  | [] ->
    if num_true acc 0 = 1 then true else false

(** [gen_pair cards loop pairing acc] returns [Pair (i, rem)], the hand to which 
    [cards] corresponds 
    Requires: [cards] is a list of cards corresponding to a pair of [i]'s, and 
      the remaining 3 cards are in the 3 card list [rem]. *)
let rec gen_pair cards loop pairing acc : hand = 
  match loop with 
  | h :: t -> 
    if is_pair_aux h cards []
    then gen_pair cards t (h :: pairing) acc
    else gen_pair cards t pairing (h :: acc)
  | [] -> let card = get_card pairing 0 0 in 
    let int = card.number in 
    let sorted = List.sort card_compare acc in 
    let stripped = strip_cards sorted [] in
    Pair (int, stripped)

(** [pair cards] returns the pair hand corresponding to cards if the cards     
    constitutes a pair, and if not, it raises exception WrongHand. *)
let pair cards : hand = 
  if is_pair cards [] then gen_pair cards cards [] [] 
  else raise (WrongHand "This hand is not of type Pair") 

(** [lone cards acc1 acc2] takes in the card list [cards] and returns the odd 
    card out in the TwoPair or one of the cards in FullHouse that is part of 
    the triple *)
let lone cards acc1 acc2 = 
  let c1 = 0 in let c2 = 0 in 
  let rec lone_aux cards count1 count2 = match cards with 
    | h :: t -> let numh = h.number in 
      if numh = acc1 then 
        if count1 < 2 then lone_aux t (count1 + 1) count2 else numh
      else if numh = acc2 then
        if count2 < 2 then lone_aux t count1 (count2 + 1) else numh
      else numh
    | [] -> failwith "lone: this is not a TwoPair or a FullHouse" in 
  lone_aux cards c1 c2

(** [has_multiple cards h] returns true if the card list [cards] contains 
    multiple cards with the same number value as the card [h]. *)
let has_mulitple_h cards h = 
  let open List in 
  let numh = h.number in 
  let card_nums = cards |> map get_number in 
  let only_h = card_nums |> filter (fun x -> x = numh) in 
  length only_h > 1

(** [is_twopair_aux cards acc1 acc2] returns [(b, (i1, i2))], where [b] is 
    [true] if the hand corresponding to [cards] is a [TwoPair], and [false] 
    otherwise.  *)
let rec is_twopair_aux cards acc1 acc2 = 
  match cards with                 
  | h :: t -> 
    let numh = h.number in if has_mulitple_h cards h 
    then begin 
      if acc1 = -1 then is_twopair_aux t numh acc2 
      else if acc2 = -1 then if acc1 <> numh then is_twopair_aux t acc1 numh
        else is_twopair_aux t acc1 acc2 
      else (true, (acc1, acc2)) end 
    else is_twopair_aux t acc1 acc2          
  | [] -> 
    let tuple = if acc1 > acc2 then (acc1, acc2) else (acc2, acc1) in 
    ((acc1 <> -1 && acc2 <> -1), tuple)

let is_twopair cards = 
  let tuple = (is_twopair_aux cards ~-1 ~-1) in 
  let ints = snd tuple in 
  let lone = lone cards (fst ints) (snd ints) in 
  fst tuple && lone <> (fst ints) && lone <> (snd ints)

(** [gen_twopair cards] returns [TwoPair (i1, i2, i3)], the hand to which 
    [cards] corresponds 
    Requires: [cards] is a list of cards corresponding to a pair of [i1]'s, a 
    pair of [i2]'s, and a [i3]. *)
let gen_twopair cards  = 
  let int_tuple = snd (is_twopair_aux cards ~-1 ~-1) in 
  let p1 = (fst int_tuple) in let p2 =  (snd int_tuple) in
  TwoPairs (p1, p2, lone cards p1 p2)

(** [twopairs cards] returns the TwoPair hand corresponding to cards if the 
    cards constitutes a two pair, and if not, it raises exception WrongHand. *)
let twopairs cards : hand =
  if is_twopair cards then gen_twopair cards 
  else raise (WrongHand "This hand is not of type TwoPairs")

(** [peek list] returns the head of the list [list] *)
let peek = function  
  | h :: t -> h  
  | _ -> failwith "nothing in list"

let is_trips cards = let open List in 
  cards |> sort_uniq card_compare |> length = 3 
  && is_twopair cards = false 

(** [gen_trips_aux cards] takes in a sorted card list [cards] and if the cards 
    constitute a Trips then it returns number of the three-of-a-kind else it 
    fails *)
let rec gen_trips_aux cards = match cards with 
  | h1 :: h2 :: t -> if h1.number = h2.number then h1.number 
    else gen_trips_aux t
  | _ -> failwith "not trip"

(** [gen_trips cards] returns [Trips (i1, i2, i3)], the hand to which [cards] 
    corresponds 
    Requires: [cards] is a list of cards corresponding to a three-of-a-kind of     
    [i1]'s, [i2], and a [i3]. *)
let gen_trips cards = let sorted = List.sort card_compare cards in 
  let three_num = gen_trips_aux sorted in 
  match List.sort_uniq card_compare cards 
        |> List.filter (fun x -> x.number <> three_num) with 
  | h1 :: h2 :: t -> Trips (three_num, h1.number, h2.number)
  | _ -> failwith "not a trip"

let is_fullhouse cards =   
  let tuple = is_twopair_aux cards ~-1 ~-1 in   
  let ints = snd tuple in   
  let lone = lone cards (fst ints) (snd ints) in  
  fst tuple && (lone = (fst ints)) || (lone = (snd ints))

(** [gen_fullhouse cards] returns [Pair (i1, i2, i3)], the hand to which [cards] 
    corresponds 
    Requires: [cards] is a list of cards corresponding to a pair of [i1]'s, a 
    pair of [i2]'s, and a [i3]. *)
let gen_fullhouse cards = match List.sort card_compare cards with 
  | [a; b; c; d; e] -> 
    if b.number = c.number then FullHouse (b.number, d.number) 
    else FullHouse (c.number, b.number)
  | _ -> failwith "gen_full_house failed"

(** [get_top_num cards] takes in a list of cards [cards] and returns
    the number of the first card. Intended for checking if a card list is a
    straight. *)
let get_top_num cards =
  match cards with 
  | h :: _ -> h.number
  | [] -> failwith "no cards in the list"

(** [is_straight_aux cards acc top_num] Takes in a sorted card list [cards]    
    (descending order), an accumulator [acc] initialized to 0, and an int     
    [top_num]which is the highest value of the cards in the card list     
    (i.e. the highest number in the straight).    
    It returns true if the cards constitute a straight, and false otherwise. *)
let rec is_straight_aux cards acc top_num =
  match cards with
  | h :: t -> 
    if h.number = (top_num - acc)
    then is_straight_aux t (acc + 1) top_num else false
  | [] -> true

(** [peek2 list] returns the second item in a list [list] *)
let peek2 = function 
  | _ :: h2 :: _ -> h2.number 
  | _ -> failwith "peek2: this list contains fewer than 2 elements"

let is_straight cards =
  let sorted = List.sort card_compare cards in
  if get_top_num sorted = 14 then 
    if peek2 sorted = 13 then is_straight_aux sorted 0 (get_top_num sorted) 
    else
    if peek2 sorted = 5
    then 
      match sorted with 
      | h :: t -> is_straight_aux t 0 (get_top_num t) 
      | _ -> failwith "is_straight: not a valid card list"
    else false 
  else is_straight_aux sorted 0 (get_top_num sorted) 

(** [gen_straight cards] returns [Straight top], the hand to which [cards] 
    corresponds 
    Requires: [cards] is a list of cards corresponding to a straight with high
    card [top]. *)
let gen_straight cards = 
  let sorted = List.sort card_compare cards in
  if get_top_num sorted = 14 then 
    if peek2 sorted = 13 then Straight 14
    else
    if peek2 sorted = 5
    then match sorted with 
      | h :: t -> Straight 5
      | _ -> failwith "gen_straight: not a valid card list"
    else failwith "gen_straight: not a straight"
  else 
    Straight (get_top_num sorted)

(** [get_flush_suit cards] takes in a list of cards [cards] and returns the suit
    of the first card. Intended for checking for flush or royal flush. *)
let get_flush_suit cards : string = 
  match cards with
  | h :: t -> h.suit
  | [] -> failwith "no cards in the list"

(** [is_flush_aux cards suit_acc] Takes in a sorted card list [cards] 
    (descending order) and the suit [suit_acc] that the cards should all be. 
    It returns true if the cards constitute a flush. *)
let rec is_flush_aux cards suit_acc = 
  match cards with
  | h :: t -> if h.suit = suit_acc then is_flush_aux t suit_acc else false
  | [] -> true

let is_flush cards : bool = 
  is_flush_aux cards (get_flush_suit cards)

(** [gen_flush pocket] returns [Flush top], the hand of which [pocket] is a 
    subset, where the remaining 3 cards of the flush are part of the table 
    cards    
    Requires: [pocket] is a list of cards corresponding to part of a flush,
    where the other 3 cards in the flush are part of the table cards. [top] is 
    the highest card in [pocket] that has the suit of the flush. *)
let gen_flush cards = 
  let sorted = List.sort card_compare cards in
  let stripped = strip_cards sorted [] in
  Flush stripped

let is_four cards = let sorted = List.sort card_compare cards in     
  let sorted_nums = sorted |> List.map (get_number) in     
  let rec first_four sort = match sort with         
    | [a; b; c; d; e] -> a = d || b = e        
    | _ -> failwith "violated precondition"  
  in first_four sorted_nums

(** [gen_four cards] returns [Four (i1, i2)], the hand to which [cards] 
    corresponds     
    Requires: [cards] is a list of cards corresponding to four [i1]'s 
    and a [i2]. *)
let gen_four cards =   let sorted = List.sort card_compare cards in     
  let sorted_nums = sorted |> List.map (get_number) in     
  let rec first_four sort : hand = match sort with         
    | [a; b; c; d; e] -> if a = d then Four (a, e) else Four (b, a)   
    | _ -> failwith "violated precondition" in  
  first_four sorted_nums

let is_straightflush cards =
  is_straight cards && is_flush cards

(** [gen_straightflush cards] returns [StraightFlush top], the hand to which 
    [cards] corresponds 
    Requires: [cards] is a list of cards corresponding to a straightflush with 
    high card [top]. *)
let gen_straightflush cards = match gen_straight cards with 
  | Straight i -> StraightFlush i
  | _ -> failwith "gen_straightflush: not a straight flush"

(** [is_royal_flush_aux cards acc suit_acc] Takes in a sorted card list [cards] 
    (descending order), an accumulator [acc] initialized to 0, 
    and the suit [suit_acc] that the cards should all be. 
    It returns true if the cards constitute a royal flush. *)
let rec is_royal_flush_aux cards acc suit_acc: bool = 
  match cards with
  | h :: t -> begin 
      if (h.number = 14 - acc) && h.suit = suit_acc then 
        is_royal_flush_aux t (acc+1) suit_acc 
      else 
        false end
  | [] -> true

(** [is_royal_flush cards] takes in a sorted card list [cards] (descending 
    order) and returns true if the cards constitute a royal flush. *)
let is_royalflush cards : bool= 
  let sorted = List.sort card_compare cards in
  is_royal_flush_aux sorted 0 (get_flush_suit cards)

let royalflush cards : hand = 
  if is_royalflush cards then RoyalFlush 
  else raise (WrongHand "This hand is not of type RoyalFlush")

let hand_of_cards cards : hand = 
  if is_royalflush cards then RoyalFlush
  else if is_straightflush cards then gen_straightflush cards
  else if is_four cards then gen_four cards
  else if is_fullhouse cards then gen_fullhouse cards
  else if is_flush cards then gen_flush cards
  else if is_straight cards then gen_straight cards
  else if is_trips cards then gen_trips cards
  else if is_twopair cards then gen_twopair cards 
  else if is_pair cards [] then gen_pair cards cards [] [] 
  else high cards

(** [rank hand] returns the rank of hand [hand], where 1 is the rank of 
    [High _], 2 is the rank of [Pair _], ..., and 10 is the rank of 
    [RoyalFlush]. *)
let rank hand : int = 
  match hand with
  | High _ -> 1
  | Pair _ -> 2
  | TwoPairs _ -> 3
  | Trips _ -> 4
  | Straight _ -> 5
  | Flush _ -> 6
  | FullHouse _ -> 7
  | Four _ -> 8
  | StraightFlush _ -> 9
  | RoyalFlush -> 10

(** [list_compare lst1 lst2] compares two sorted card lists and returns the one
    the has the card with the highest number that is not in the other list. 
    Returns [] if the lists are   *)
let rec list_compare lst1 lst2 = match lst1, lst2 with 
  | h1 :: t1, h2 :: t2 -> if h1 > h2 then 1 else if h1 < h2 then 
      -1 else list_compare t1 t2 
  | [], [] -> 0
  | _ -> failwith "list_compare: lists have different lengths"

(** [high_compare high1 high2] checks if the high1 is greater than high2. If so, 
    it returns 1. If they are equal return 0. If neither, return -1. *)
let high_compare high1 high2 = match high1, high2 with 
  | High lst1, High lst2 -> list_compare lst1 lst2 
  | _ -> failwith "high_compare: not high cards"

(** [pair_compare pair1 pair2] checks if the pair1 is greater than pair2. If so, 
    it returns 1. If they are equal return 0. If neither, return -1. *)
let pair_compare pair1 pair2 = match pair1, pair2 with 
  | Pair (p1, lst1), Pair (p2, lst2) -> let value = compare p1 p2 in 
    if value = 0 then list_compare lst1 lst2 else value
  | _ -> failwith "pair_compare: arguments are not both pairs"

(** [twopair_compare two1 two2] checks if the two1 is greater than two2. If so, 
    it returns 1. If they are equal return 0. If neither, return -1.   
    Requires: [two1] and [two2] are both TwoPairs *)
let twopair_compare two1 two2 = match two1, two2 with 
  | TwoPairs (p1, p2, p3), TwoPairs (p4, p5, p6) -> let value = compare p1 p4 in 
    if value = 0 then 
      let value' = compare p2 p5 in 
      if value' = 0 then compare p3 p6 
      else value' 
    else value
  | _ -> failwith "Violated precondition"

(** [trips_compare hand1 hand2] checks if hand1 is greater than hand2. If so, 
    it returns 1. If they are equal return 0. If neither, return -1.    
    Requires: [hand1] and [hand2] are both Trips *)
let trips_compare hand1 hand2 = match hand1, hand2 with
  | Trips (i1, i2, i3), Trips (i4, i5, i6) ->
    let compare_3 = compare i1 i4 in 
    if compare_3 = 0 then 
      let compare_3_2 = compare i2 i5 in
      if compare_3_2 = 0 then 
        compare i3 i6      
      else compare_3_2
    else compare_3
  | _ -> failwith "trips_compare : not a Trip"

(** [straight_compare hand1 hand2] checks if hand1 is greater than hand2. If so, 
    it returns 1. If they are equal return 0. If neither, return -1.    
    Requires: [hand1] and [hand2] are both Straight or both StraightFlush *)
let straight_compare hand1 hand2 = match hand1, hand2 with 
  | Straight i1, Straight i2 
  | StraightFlush i1, StraightFlush i2 -> compare i1 i2
  | _ -> failwith "straight_compare : not a straight"

(** [flush_compare hand1 hand2] checks if hand1 is greater than hand2. If so, 
    it returns 1. If they are equal return 0. If neither, return -1.    
    Requires: [hand1] and [hand2] are both Flush *)
let flush_compare hand1 hand2 = match hand1, hand2 with
  | Flush lst1, Flush lst2 -> list_compare lst1 lst2
  | _ -> failwith "flush_compare : not a flush"

(** [fullhouse_compare hand1 hand2] checks if hand1 is greater than hand2. 
    If so, it returns 1. If they are equal return 0. If neither, return -1.    
    Requires: [hand1] and [hand2] are both FullHouse *)
let fullhouse_compare hand1 hand2 = match hand1, hand2 with
  | FullHouse (i1, i2), FullHouse (i3, i4) -> 
    let compare_fh = compare i1 i3 in
    if compare_fh = 0 then compare i2 i4
    else compare_fh
  | _ -> failwith "fullhouse_compare : not a fullhouse"

(** [four_compare hand1 hand2] checks if hand1 is greater than hand2. If so, it 
    returns 1. If they are equal return 0. If neither, return -1.    
    Requires: [hand1] and [hand2] are both Four *)
let four_compare hand1 hand2 = match hand1, hand2 with
  | Four (i1, i2), Four (i3, i4) -> let compare_4 = compare i1 i3 in
    if compare_4 = 0 then compare i2 i4
    else compare_4
  | _ -> failwith "four_compare : not a four"

(** [hand_compare hand1 hand2] compares [hand1] and [hand2], which have the 
    same hand type constructor (e.g., they're both straights), and returns:
    1 if [hand1] is better than [hand2]
    -1 if [hand2] is better than [hand1]
    0 otherwise. *)
let hand_compare hand1 hand2 : int = 
  let rank1 = rank hand1 in let rank2 = rank hand2 in 
  if rank1 > rank2 then 1 else if rank1 < rank2 then -1 else 
    match hand1 with 
    | RoyalFlush -> 0
    | StraightFlush i -> straight_compare hand1 hand2
    | Four (i1, i2) -> four_compare hand1 hand2
    | FullHouse (i1, i2) -> fullhouse_compare hand1 hand2
    | Flush lst -> flush_compare hand1 hand2
    | Straight i -> straight_compare hand1 hand2
    | Trips (i1, i2, i3) -> trips_compare hand1 hand2
    | TwoPairs (i1, i2, i3) -> twopair_compare hand1 hand2
    | Pair (i1, i_lst) -> pair_compare hand1 hand2
    | High i_lst -> high_compare hand1 hand2

(** [bestfive cards hand_acc list_acc] returns the five cards in [cards] that 
    make up the best possible hand *)
let rec best_five (cards : (card list) list) (hand_acc : hand) 
    (list_acc : card list) : card list = 
  match cards with  
  | h :: t -> 
    let h_hand = hand_of_cards h in 
    let value = hand_compare h_hand hand_acc in if value = 1 
    then best_five t h_hand h 
    else best_five t hand_acc list_acc
  | [] -> list_acc 

let best_hand (table : card list) (pocket : card list) : hand = 
  let cards = table @ pocket in 
  let combos = possible_hands combinations cards [] in
  let first = hand_of_cards (peek combos)  in 
  let five = best_five combos first (peek combos) in
  hand_of_cards five

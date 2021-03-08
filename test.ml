open Cards
open OUnit2
open State
open Ai

(**
   Test Plan:

    Our test.ml file (OUnit) tests the following: 
    Cards module: deck/card functions ([deal], [shuffle]), 
      hand functions ([best_hand], checkers, generators, and comparisons)
    State module: betting functions ([bet], [call], [check], [raise_bet], 
      [fold], [all_in], side pots ([create_sidepots], [check_all_ins], 
      several helpers* )
    AI module: [monte_carlo]**
    Partial Integration

   Our test series generally consist of a few unit tests and black box tests 
   followed by several glass box tests. When implementing functions, we would 
   most often begin by writing a couple of simple black box tests, before 
   actually writing the function (as in TDD). We then proceeded to write glass 
   box tests (covering conditional branches, match cases, boundary cases, 
   exceptions, etc.) after completing some implementation. We also constructed 
   tests with the help of bisect, so that we could be sure to cover as many 
   outcomes as possible. Furthermore, we ran through playthroughs frequently 
   and added tests when things behaved abnormally or unexpectedly. With these 
   combined methods for coming up with tests, we believe our automatic 
   testing to be exhaustive. 
  *There are several “helper functions” for [create_sidepots] and 
   [check_all_ins]. Despite the testing of helper functions often being omitted,
   we included tests for a few of these functions as it proved necessary for 
   debugging.
  **We wrote tests for monte_carlo not to check the expected output exactly 
   (as the simulations are random), but to make sure the function ran through
   all the way and output the correct type. Hence, these tests have been 
   acommented out. We manually compared outputs to online poker odds
   calculators.

   The following were manually tested (via playthroughs, traces, and watches):
   State module: [winner], [distribute]
   AI module: [odds] and related odds functions
   Main: interface and integration
*)



(** [pp_card c] pretty-prints card [c]. *)
let pp_card (c : card) = 
  "\"" ^ ((get_number c) |> string_of_int) ^ " " ^ (get_suit c) ^ "\""

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



let pp_player (p : player) = 
  "\"" ^ (get_name p) ^ " " ^ ((get_money p) |> string_of_int) ^ " " ^ 
  ((get_pocket p) |> pp_list pp_card) ^ " " ^ ((get_folded p) |> string_of_bool)
  ^ " " ^ ((get_money_bet p) |> string_of_int)^ " " ^
  ((get_still_betting p) |> string_of_bool) ^"\""

let pp_pot pot = "\"" ^ "(" ^ (fst pot |> string_of_int) ^ ", " ^  
                 pp_list pp_player (snd pot) ^ ")" ^ "\""

let pp_string_pot pot = "\"" ^ "(" ^ (fst pot |> string_of_int) ^ ", " ^  
                        pp_list (fun x -> x) (snd pot) ^ ")" ^ "\""

let pp_state (s : State.t) = 
  "\"" ^ "Dealer: " ^((get_dealer s) |> string_of_int) ^ " Flop: " ^ 
  ((get_flop s) |> pp_list pp_card) ^ " Turn: " ^
  ((get_turn s) |> pp_list pp_card) ^ " River: " ^
  ((get_river s) |> pp_list pp_card) ^ " Pot: " ^
  ((get_pot s) |> string_of_int) ^ " HRB: " ^
  ((get_hrb s) |> string_of_int) ^ " Player array: " ^
  ((get_player_array s) |> Array.to_list |> pp_list pp_player) 
  ^ " Player betting: " ^ (get_player_betting s |> string_of_int) 
  ^ " Side pots: " ^ (get_side_pots s |> pp_list pp_string_pot) ^
  "\""


(** Removes 2 cards for every player in the player array*)
let rec shed (deck : card list) (acc : int) (length : int) =  if acc != length 
  then match deck with  
    | [] -> raise (NotEnoughCards "Not enough cards")
    | h1 :: h2 :: t -> shed t (acc + 1) length
    | h :: t -> raise (NotEnoughCards "Not enough cards")
  else deck

(** Runs deal on a [deck] input and checks that the deck returned from that 
    matches some provided expected output*)
let deal_test 
    (name : string)
    (deck : card list) 
    (player_array : player array)
    (acc : int)
    (expected_output : card list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (deal deck player_array 0) 
        ~printer:(pp_list pp_card))

let (players1 : player array) = [|(player_init "Abhyuday" 50); 
                                  (player_init "Dhruv" 250); 
                                  (player_init "Hali" 99999)|]
let players_short = [|(player_make "A" 10 [] false 0 true); 
                      (player_make "B" 250 [] false 0 true)|]
let tiny_deck_0_left = [(card_make "Spades" 2); (card_make "Spades" 3); 
                        (card_make "Spades" 4); (card_make "Spades" 5);]
let tiny_deck_1_left =[(card_make "Spades" 2); (card_make "Spades" 3); 
                       (card_make "Spades" 4)]

(** used for testing player 0's cards*)
let deck3 = deal tiny_deck_0_left players_short 0 

let shuffled_deck1 = shuffle std_deck

let shuffle_tests = [

  (*Black*)
  "Testing that shuffling the standard deck gives a different deck each time" 
  >:: (fun _ -> assert_equal (shuffle std_deck = shuffle std_deck) false);
  (*Black*)
  "Testing for duplicates" >:: (fun _ -> assert_equal 52 
                                   ((shuffle std_deck 
                                     |> List.sort_uniq compare) 
                                    |> List.length));
]

let deal_tests = [
  (*Unit: Black*)
  deal_test "unit test dealing out the std_deck" std_deck players1 0 
    (shed std_deck 0 (Array.length players1));
  deal_test "unit test dealing out non-standard deck" shuffled_deck1 players1 0 
    (shed shuffled_deck1 0 (Array.length players1));

  (*Glass*)
  "Raising exception when we are left with empty list in [deal]" >:: 
  (fun _ -> assert_raises (NotEnoughCards "Too many players") (fun () -> 
       deal tiny_deck_0_left players1 0));
  "Raising exception when we are left with a list with one element from [deal]" 
  >:: (fun _ -> assert_raises (NotEnoughCards "Too many players") (fun () -> 
      deal tiny_deck_1_left players1 0));

  (*Black*)
  "Make sure the first player's pocket is updated properly" >:: 
  (fun _ ->  assert_equal (get_pocket (Array.get players_short 0)) 
      [card_make "Spades" 2; card_make "Spades" 3]) 
]


let bet_test 
    (name : string)
    (player_betting : int) 
    (amount : int)
    (state1 : State.t)
    (newpb : int)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (bet player_betting amount state1 newpb) 
        ~printer:(pp_state))

let call_test 
    (name : string)
    (player_betting : int) 
    (state1 : State.t)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (call player_betting state1) 
        ~printer:(pp_state))

let check_test 
    (name : string)
    (player_betting : int) 
    (state1 : State.t)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check player_betting state1) 
        ~printer:(pp_state))

let raise_test 
    (name : string)
    (player_betting : int) 
    (amt : int)
    (state1 : State.t)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (raise_bet player_betting amt state1) 
        ~printer:(pp_state))

let fold_test 
    (name : string)
    (player_betting : int) 
    (state1 : State.t)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (fold player_betting state1) 
        ~printer:(pp_state))

let all_in_test 
    (name : string)
    (player_betting : int) 
    (state1 : State.t)
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (all_in player_betting state1) 
        ~printer:(pp_state))

let (players_path_1 : player array) = [|(player_init "Abhyuday" 50); 
                                        (player_init "Dhruv" 250); 
                                        (player_init "Hali" 99999)|]

let unit_state = init_state shuffled_deck1 players_path_1

let path_1_bet_1 = [|(player_make "Abhyuday" 40 [] false 10 true); 
                     (player_make "Dhruv" 250 [] false 0 true); 
                     (player_make "Hali" 99999 [] false 0 true)|]
let path_1_bet_2 = [|(player_make "Abhyuday" 40 [] false 10 true); 
                     (player_make "Dhruv" 230 [] false 20 true); 
                     (player_make "Hali" 99999 [] false 0 true)|]
let path_1_call_1 = [|(player_make "Abhyuday" 40 [] false 10 true); 
                      (player_make "Dhruv" 230 [] false 20 true); 
                      (player_make "Hali" 99979 [] false 20 true)|]
let path_1_raise_1 = [|(player_make "Abhyuday" 20 [] false 30 true); 
                       (player_make "Dhruv" 230 [] false 20 true); 
                       (player_make "Hali" 99979 [] false 20 true)|]
let path_1_all_in_1 = [|(player_make "Abhyuday" 20 [] false 30 true); 
                        (player_make "Dhruv" 0 [] false 250 true); 
                        (player_make "Hali" 99979 [] false 20 true)|]

let players_path_2 = [|(player_make "Abhyuday" 50 [] false 0 true); 
                       (player_make "Dhruv" 250 [] false 0 true); 
                       (player_make "Hali" 99999 [] false 0 true)|]
let path_2_fold_1 = [|(player_make "Abhyuday" 50 [] false 0 true); 
                      (player_make "Dhruv" 250 [] true 0 false); 
                      (player_make "Hali" 99999 [] false 0 true)|]

let get_flop = function 
  | h1 :: h2 :: h3 :: t -> (h1 :: h2 :: h3 :: [], t)
  | h :: t -> failwith "bad"
  | [] -> failwith "bad"
let get_turn_and_river = function
  | h1 :: h2  :: t -> (h1 :: [],  h2 :: [])
  | h :: t -> failwith "bad"
  | [] -> failwith "bad"

let flop = get_flop shuffled_deck1
let turn = get_turn_and_river (snd flop)

let check_state1 = init_state shuffled_deck1 players_path_2

let check_state2 = update_state 0 (fst flop) (fst turn) (snd turn)
    0 0 players_path_2 1 
    [(0, players_path_2 |> Array.to_list |> List.map get_name)] 

let fold_state1 = update_state 0 (fst flop) (fst turn) (snd turn)
    0 0 path_2_fold_1 2 
    [(0, path_2_fold_1 |> Array.to_list |> List.map get_name)] 

let bet_state1 = update_state 0 (fst flop) (fst turn) 
    (snd turn) 10 10 path_1_bet_1 1 
    [(0, path_1_bet_1 |> Array.to_list |> List.map get_name)] 

let bet_state2 = update_state 0 (fst flop) (fst turn)
    (snd turn) 30 20 path_1_bet_2 2 
    [(0, path_1_bet_2 |> Array.to_list |> List.map get_name)] 

let call_state1 = update_state 0 (fst flop) (fst turn)
    (snd turn) 50 20 path_1_call_1 0 
    [(0, path_1_call_1 |> Array.to_list |> List.map get_name)] 

let raise_state1 = update_state 0 (fst flop) (fst turn) 
    (snd turn) 70 30 path_1_raise_1 1 
    [(0, path_1_raise_1 |> Array.to_list |> List.map get_name)] 

let all_in_state1 = update_state 0 (fst flop) (fst turn) 
    (snd turn) 300 250 path_1_all_in_1 2 
    [(0, path_1_all_in_1 |> Array.to_list |> List.map get_name)] 

let bet_tests = [
  bet_test "Abhyuday betting 10 moneys" 0 10 unit_state 1 bet_state1;
  bet_test "Dhruv betting 20 moneys" 1 20 bet_state1 2 bet_state2;
  call_test "Hali calling (20 moneys)" 2 bet_state2 call_state1;
  raise_test "Abhyuday raising by 10" 0 10 call_state1 raise_state1;
  all_in_test "Dhruv goes all-in" 1 raise_state1 all_in_state1;
  check_test "Abhyuday checks" 0 check_state1 check_state2;
  fold_test "Dhruv folds :(" 1 check_state2 fold_state1;
]

let uno = player_make "uno" 50 [] true 50 false
let dos = player_make "dos" 0 [] false 200 true
let tres = player_make "tres" 100 [] false 200 true 
let players_path_3 : player array = [|uno ; dos ; tres|]

let pp_3_list = Array.to_list players_path_3 

let pp_3_sidepots = [(450, get_name dos :: get_name tres :: [])]

let pp_3_minbet = (200, dos :: tres :: [])

let pp_3_sidepot = (450, get_name dos :: get_name tres :: [])


let uno_output = player_make "uno" 50 [] true 0 false
let dos_output = player_make "dos" 0 [] false 100 true
let tres_output = player_make "tres" 100 [] false 100 true 
let players_path_3_output : player list = 
  [|uno_output ; dos_output ; tres_output|] |> Array.to_list

let uno_zero = player_make "uno" 50 [] true 0 false
let dos_zero = player_make "dos" 0 [] false 0 true
let tres_zero = player_make "tres" 100 [] false 0 true 
let pp_3_zero : player list = [|uno_zero ; dos_zero ; tres_zero|] 
                              |> Array.to_list

(* More complicated tests for create_sidepots_aux *)
let player_all_in_50 = player_make "all_in_50" 0 [] false 50 true
let player_raise_100 = player_make "raise_100" 200 [] false 150 true
let player_fold = player_make "fold" 100 [] true 0 false
let player_all_in_25 = player_make "all_in_25" 0 [] false 25 true
let player_call_150 = player_make "call_150" 500 [] false 150 true

let players_path_4 = [|player_all_in_50; player_raise_100; player_fold; 
                       player_all_in_25; player_call_150|]
let pp_4_list = Array.to_list players_path_4 

let pp_4_sidepots = [(200, 
                      [get_name player_raise_100; get_name player_call_150]);
                     (75, [get_name player_all_in_50; get_name player_raise_100; 
                           get_name player_call_150]);
                     (100, [get_name player_all_in_50; 
                            get_name player_raise_100; 
                            get_name player_all_in_25; 
                            get_name player_call_150])]


let check_if_bets_equal_test
    (name : string)
    (players : player list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_if_bets_equal players) 
        ~printer:(string_of_bool))

(* let check_if_zero_money_test *)

let find_min_bet_test
    (name : string)
    (players : player list) 
    (expected_output : (int * player list)) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (find_min_bet players (500, [])) 
        ~printer:(pp_pot))

let create_single_sidepot_test
    (name : string)
    (players : player list) 
    (min : int)
    (pot : int)
    (acc : string list)
    (expected_output : (int * string list)) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (create_single_sidepot players min pot acc) 
        ~printer:(pp_string_pot))

let decrease_test 
    (name : string)
    (players : player list) 
    (money : int)
    (acc : player list)
    (expected_output : player list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (decrease players money acc) 
        ~printer:(pp_list pp_player))

let check_all_zero_money_bet_test
    (name : string)
    (players : player list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_all_zero_money_bet players) 
        ~printer:(string_of_bool))

let create_sidepots_aux_test
    (name : string)
    (players : player list) 
    (acc : (int * string list) list)
    (expected_output : (int * string list) list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (create_sidepots_aux players acc) 
        ~printer:(pp_list pp_string_pot))

let first_rnd_1 = player_make "Bets 50" 50 [] false 50 true 
let first_rnd_2 = player_make "Calls 50" 100 [] false 50 true 
let first_rnd_3 = player_make "Calls calls 50" 150 [] false 50 true 
let first_rnd_4 = player_make "Calls^3 50" 150 [] false 50 true 

let first_rnd_start = [|first_rnd_1; first_rnd_2; first_rnd_3; first_rnd_4|]

let st_all_ins1 = quick_update (init_state shuffled_deck1 first_rnd_start) 
    200 50 first_rnd_start 0

let first_rnd_1_end = player_make "Bets 50" 50 [] false 0 true 
let first_rnd_2_end = player_make "Calls 50" 100 [] false 0 true 
let first_rnd_3_end = player_make "Calls calls 50" 150 [] false 0 true 
let first_rnd_4_end = player_make "Calls^3 50" 150 [] false 0 true 
let first_rnd_end = 
  [|first_rnd_1_end; first_rnd_2_end; first_rnd_3_end; first_rnd_4_end|]
let sps1 = [(200, ["Bets 50"; "Calls 50"; "Calls calls 50"; "Calls^3 50"])]
let st_all_ins1_exp = quick_update (side_pot_update st_all_ins1 sps1) 200 0 
    first_rnd_end 0

(* players during round 2 before check_all_ins: *)
let snd_rnd_1 = 
  player_update first_rnd_1_end 0 50 false true (* "Bets 50" *)
let snd_rnd_2 = 
  player_update first_rnd_2_end 50 50 false true (* "Calls 50" *)
let snd_rnd_3 = 
  player_update first_rnd_3_end 100 50 false true (* "Calls calls 50" *)
let snd_rnd_4 = 
  player_update first_rnd_4_end 100 50 false true (* "Calls^3 50" *)
let snd_rnd = [|snd_rnd_1; snd_rnd_2; snd_rnd_3; snd_rnd_4|]
let snd_rnd_st = quick_update st_all_ins1_exp 400 50 snd_rnd 0

(* players at end of round 2, after one player goes all-in, and rest check: *)
let snd_rnd_1_end = 
  player_update snd_rnd_1 0 0 false false (* "Bets 50" *)
let snd_rnd_2_end = 
  player_update snd_rnd_2 50 0 false true (* "Calls 50" *)
let snd_rnd_3_end = 
  player_update snd_rnd_3 100 0 false true (* "Calls calls 50" *)
let snd_rnd_4_end = 
  player_update snd_rnd_4 100 0 false true (* "Calls^3 50" *)
let snd_rnd_end = [|snd_rnd_1_end; snd_rnd_2_end; snd_rnd_3_end; snd_rnd_4_end|]

let sps2 = [(0, ["Calls 50"; "Calls calls 50"; "Calls^3 50"]); 
            (400, ["Bets 50"; "Calls 50"; "Calls calls 50"; "Calls^3 50"])]
let st_all_ins2_exp = quick_update (side_pot_update st_all_ins1_exp sps2) 0 0 
    snd_rnd_end 0


(* players for second else branch: *)
let thrd_rnd = [|player_all_in_50; player_raise_100; player_fold; 
                 player_all_in_25; player_call_150|]

let thrd_rnd_sps = [(200, 
                     [get_name player_raise_100; get_name player_call_150]);
                    (75, [get_name player_all_in_50; get_name player_raise_100; 
                          get_name player_call_150]);
                    (100, [get_name player_all_in_50; get_name player_raise_100; 
                           get_name player_all_in_25; 
                           get_name player_call_150])]

let st_all_ins3 = quick_update (init_state shuffled_deck1 thrd_rnd) 375 150 
    thrd_rnd 0
let thrd_rnd_end = [|player_update player_all_in_50 0 0 false false; 
                     player_update player_raise_100 200 0 false true; 
                     player_update player_fold 100 0 true false; 
                     player_update player_all_in_25 0 0 false false;
                     player_update player_call_150 500 0 false true|]
let st_all_ins3_exp = quick_update (side_pot_update st_all_ins3 thrd_rnd_sps) 
    200 0 thrd_rnd_end 0

let player_all_in_50' = player_make "all_in_50" 50 [] false 0 true
let player_raise_50' = player_make "raise_100" 350 [] false 0 true
let player_fold' = player_make "fold" 100 [] false 0 true
let player_all_in_25' = player_make "all_in_25" 25 [] false 0 true
let player_call_150' = player_make "call_150" 650 [] false 0 true
let integration_array = [|player_all_in_50'; player_raise_50'; player_fold'; 
                          player_all_in_25'; player_call_150'|]
let integration_state_init = init_state shuffled_deck1 integration_array
let integration_state = integration_state_init |> all_in 0 |> raise_bet 1 100 
                        |> fold 2 |> all_in 3 |> call 4

let check_all_ins_test
    (name : string)
    (st : State.t) 
    (expected_output : State.t) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (check_all_ins st) 
        ~printer: pp_state)

let still_betting_players players = 
  List.filter (fun x -> get_still_betting x) players

let round_end_tests = [
  decrease_test "Decrease everyone's money_bet by 100" pp_3_list 100 [] 
    players_path_3_output;
  check_if_bets_equal_test "Players 2 and 3 have equal bets, should be true" 
    (pp_3_list |> still_betting_players) true;
  check_all_zero_money_bet_test "not all zero" pp_3_list false;
  check_all_zero_money_bet_test "all zero" pp_3_zero true;
  create_sidepots_aux_test "sp of 450, one folded" pp_3_list 
    [] pp_3_sidepots;
  create_sidepots_aux_test "3 sps, 200, 75, 100, one folded" 
    pp_4_list [] pp_4_sidepots;
  find_min_bet_test "200" (pp_3_list |> still_betting_players) 
    pp_3_minbet;
  create_single_sidepot_test "test" pp_3_list 200 0 [] 
    pp_3_sidepot;
  check_all_ins_test "testing all in, first else branch" 
    st_all_ins1 st_all_ins1_exp;
  check_all_ins_test "testing all in, first if branch" 
    snd_rnd_st st_all_ins2_exp;
  check_all_ins_test "testing all in, second else branch" 
    st_all_ins3 st_all_ins3_exp;
  check_all_ins_test "testing all in, with integration" 
    integration_state st_all_ins3_exp;
]

let high1 = 
  [card_make "Hearts" 4; 
   card_make "Clubs" 6;
   card_make "Diamonds" 7; 
   card_make "Clubs" 2;  
   card_make "Spades" 3]
let high1_hand = high_make [7; 6; 4; 3; 2]

let high1_equal = 
  [card_make "Diamonds" 4; 
   card_make "Spades" 6;
   card_make "Diamonds" 7; 
   card_make "Clubs" 2;  
   card_make "Spades" 3]
let high1_hand_equal = high_make [7; 6; 4; 3; 2]

let high2 = 
  [card_make "Hearts" 5; 
   card_make "Clubs" 6;
   card_make "Diamonds" 7; 
   card_make "Clubs" 2;  
   card_make "Spades" 3]
let high2_hand = high_make [7; 6; 5; 3; 2]

let high3 = 
  [card_make "Hearts" 7; 
   card_make "Clubs" 9;
   card_make "Diamonds" 10; 
   card_make "Clubs" 5;  
   card_make "Spades" 3]
let high3_hand = high_make [10; 9; 7; 5; 3]

let high4 = 
  [card_make "Hearts" 7; 
   card_make "Clubs" 9;
   card_make "Diamonds" 10; 
   card_make "Clubs" 5;  
   card_make "Spades" 4]
let high4_hand = high_make [10; 9; 7; 5; 4]

let pair1 = 
  [card_make "Hearts" 4; 
   card_make "Clubs" 5;
   card_make "Hearts" 2; 
   card_make "Clubs" 2;  
   card_make "Spades" 3]
let pair1_hand = pair_make 2 [5; 4; 3]

let twopairs1 = 
  [card_make "Hearts" 14; 
   card_make "Clubs" 14; 
   card_make "Hearts" 2; 
   card_make "Clubs" 2; 
   card_make "Clubs" 3]
let twopairs1_hand = twopairs_make 14 2 3

let twopairs2 = 
  [card_make "Hearts" 14; 
   card_make "Clubs" 2; 
   card_make "Hearts" 3; 
   card_make "Clubs" 14; 
   card_make "Hearts" 2]
let twopairs2_hand = twopairs1_hand 

let twopairs3 = 
  [card_make "Hearts" 12; 
   card_make "Clubs" 3; 
   card_make "Hearts" 7; 
   card_make "Hearts" 3; 
   card_make "Clubs" 12]
let twopairs3_hand = twopairs_make 12 3 7

let twopairs4 = 
  [card_make "Hearts" 12; 
   card_make "Clubs" 4; 
   card_make "Hearts" 7; 
   card_make "Hearts" 4; 
   card_make "Clubs" 12]
let twopairs4_hand = twopairs_make 12 4 7


let twopairs5 = 
  [card_make "Hearts" 12; 
   card_make "Clubs" 4; 
   card_make "Hearts" 8; 
   card_make "Hearts" 4; 
   card_make "Clubs" 12]
let twopairs5_hand = twopairs_make 12 4 8


let straight = 
  [card_make "Hearts" 14; 
   card_make "Clubs" 2; 
   card_make "Hearts" 3; 
   card_make "Clubs" 4; 
   card_make "Hearts" 5]
let straight_hand = straight_make 5

let straight2 = 
  [card_make "Hearts" 6; 
   card_make "Clubs" 4; 
   card_make "Hearts" 3; 
   card_make "Clubs" 7; 
   card_make "Hearts" 5]
let straight_hand2 = straight_make 7

let straight3 = 
  [ card_make "Hearts" 13;
    card_make "Diamonds" 14; 
    card_make "Diamonds" 10; 
    card_make "Diamonds" 12; 
    card_make "Diamonds" 11;]
let straight_hand3 = straight_make 14

let royalflush = 
  [card_make "Spades" 14; 
   card_make "Spades" 12; 
   card_make "Spades" 11; 
   card_make "Spades" 13; 
   card_make "Spades" 10]
let royalflush_hand = royalflush_make

let four_kind = 
  [card_make "Hearts" 4; 
   card_make "Clubs" 4; 
   card_make "Diamonds" 4; 
   card_make "Spades" 4; 
   card_make "Diamonds" 10]
let four_hand = four_make 4 10

let four_kind2 = 
  [card_make "Hearts" 4; 
   card_make "Clubs" 4; 
   card_make "Diamonds" 13; 
   card_make "Spades" 4; 
   card_make "Diamonds" 4]
let four_hand2 = four_make 4 13

let full_house = 
  [card_make "Hearts" 4; 
   card_make "Clubs" 4; 
   card_make "Diamonds" 13; 
   card_make "Spades" 13; 
   card_make "Diamonds" 4]
let fullhouse_hand = fullhouse_make 4 13 

let fuller_house = 
  [card_make "Hearts" 6; 
   card_make "Clubs" 10; 
   card_make "Spades" 6; 
   card_make "Hearts" 10; 
   card_make "Diamonds" 6]
let fullerhouse_hand = fullhouse_make 6 10 

let flush = [
  card_make "Hearts" 4; 
  card_make "Hearts" 6; 
  card_make "Hearts" 13; 
  card_make "Hearts" 8; 
  card_make "Hearts" 2
]
let flush_hand = flush_make [13; 8; 6; 4; 2]

let flush2 = [
  card_make "Clubs" 3; 
  card_make "Clubs" 4; 
  card_make "Clubs" 13; 
  card_make "Clubs" 14; 
  card_make "Clubs" 8
]
let flush_hand2 = flush_make [14; 13; 8; 4; 3]

let straight_flush = 
  [ card_make "Diamonds" 3;
    card_make "Diamonds" 7; 
    card_make "Diamonds" 6; 
    card_make "Diamonds" 4; 
    card_make "Diamonds" 5;]
let straightflush_hand = straightflush_make 7

let straight_flush2 = 
  [ card_make "Diamonds" 2;
    card_make "Diamonds" 3; 
    card_make "Diamonds" 5; 
    card_make "Diamonds" 14; 
    card_make "Diamonds" 4;]
let straightflush_hand2 = straightflush_make 5


let three_kind1 = 
  [card_make "Hearts" 4; 
   card_make "Clubs" 4; 
   card_make "Diamonds" 13; 
   card_make "Hearts" 10; 
   card_make "Diamonds" 4]
let three_hand1 = trips_make 4 13 10 

let three_kind2 = 
  [card_make "Hearts" 5; 
   card_make "Clubs" 3; 
   card_make "Spades" 5; 
   card_make "Hearts" 10; 
   card_make "Diamonds" 5]
let three_hand2 = trips_make 5 10 3

let three_kind3 = 
  [card_make "Hearts" 12; 
   card_make "Clubs" 6; 
   card_make "Spades" 6; 
   card_make "Hearts" 5; 
   card_make "Diamonds" 6]
let three_hand3 = trips_make 6 12 5

let three_kind4 = 
  [card_make "Hearts" 14; 
   card_make "Clubs" 7; 
   card_make "Spades" 10; 
   card_make "Hearts" 7; 
   card_make "Diamonds" 7]
let three_hand4 = trips_make 7 14 10 

let high_test
    (name : string)
    (cards : card list) 
    (expected_output : hand) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (high cards) 
        ~printer: pp_hand)

let pair_test
    (name : string)
    (cards : card list) 
    (expected_output : hand) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (pair cards) 
        ~printer: pp_hand)

let is_twopair_test 
    (name : string)
    (cards : card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_twopair cards) 
        ~printer: string_of_bool)

let is_trips_test 
    (name : string)
    (cards : card list)
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_trips cards) 
        ~printer: string_of_bool)

let is_straight_test
    (name : string)
    (cards : card list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_straight cards) 
        ~printer: string_of_bool)

let is_flush_test
    (name : string)
    (cards : card list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_flush cards) 
        ~printer: string_of_bool)

let is_fullhouse_test
    (name : string)
    (cards : card list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_fullhouse cards ) 
        ~printer: string_of_bool)

let is_four_test
    (name : string)
    (cards : card list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_four cards) 
        ~printer: string_of_bool)

let is_straightflush_test
    (name : string)
    (cards : card list) 
    (expected_output : bool) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (is_straightflush cards) 
        ~printer: string_of_bool)

let royalflush_test
    (name : string)
    (cards : card list) 
    (expected_output : hand) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (Cards.royalflush cards) 
        ~printer: pp_hand)

let table_cards1 = 
  [card_make "Hearts" 10;
   card_make "Hearts" 6;
   card_make "Diamonds" 3;
   card_make "Spades" 5;
   card_make "Hearts" 12
  ]

let pocket1 = 
  [card_make "Diamonds" 6;
   card_make "Clubs" 5]
let best_hand1 = twopairs_make 6 5 12 

let pocket2 = 
  [card_make "Hearts" 5;
   card_make "Hearts" 4]
let best_hand2 = flush_make [12; 10; 6; 5; 4] 

let pocket3 = 
  [card_make "Spades" 7;
   card_make "Clubs" 4]
let best_hand3 = straight_make 7

let pocket4 = 
  [card_make "Hearts" 7;
   card_make "Clubs" 12]
let best_hand4 = pair_make 12 [10; 7; 6]

let pocket5 = 
  [card_make "Hearts" 13;
   card_make "Clubs" 14]
let best_hand5 = high_make [14; 13; 12; 10; 6] 


let table_cards2 = 
  [card_make "Hearts" 10;
   card_make "Hearts" 9;
   card_make "Clubs" 9;
   card_make "Hearts" 12;
   card_make "Hearts" 13]
let pocket2_1 = 
  [card_make "Hearts" 11; 
   card_make "Hearts" 14]
let best_hand2_1 = royalflush_make 
let pocket2_2 = 
  [card_make "Hearts" 11; 
   card_make "Clubs" 14]
let best_hand2_2 = straightflush_make 13
let pocket2_3 = 
  [card_make "Clubs" 11; 
   card_make "Clubs" 14]
let best_hand2_3 = straight_make 14

let table_cards3 = 
  [card_make "Hearts" 6;
   card_make "Hearts" 5;
   card_make "Clubs" 4;
   card_make "Hearts" 8;
   card_make "Hearts" 7]

let pocket3_1 = 
  [card_make "Clubs" 3; 
   card_make "Clubs" 9]
let best_hand3_1 = straight_make 9

let pocket3_2 = 
  [card_make "Clubs" 10; 
   card_make "Clubs" 9]
let best_hand3_2 = straight_make 10

let pocket3_3 = 
  [card_make "Clubs" 2; 
   card_make "Clubs" 3]
let best_hand3_3 = straight_make 8

let table_cards4 = 
  [card_make "Spades" 3;
   card_make "Spades" 11;
   card_make "Spades" 9;
   card_make "Spades" 7;
   card_make "Spades" 12]

let pocket4_1 = 
  [card_make "Clubs" 12; 
   card_make "Diamonds" 3]
let best_hand4_1 = flush_make [12; 11; 9; 7; 3]

let pocket4_2 = 
  [card_make "Spades" 2; 
   card_make "Hearts" 13]
let best_hand4_2 = flush_make [12; 11; 9; 7; 3]

let pocket4_3 = 
  [card_make "Spades" 13; 
   card_make "Clubs" 13]
let best_hand4_3 = flush_make [13; 12; 11; 9; 7]

let table_cards5 = 
  [card_make "Hearts" 3;
   card_make "Spades" 11;
   card_make "Spades" 9;
   card_make "Spades" 7;
   card_make "Spades" 14]

let pocket5_1 = 
  [card_make "Spades" 12; 
   card_make "Diamonds" 3]
let best_hand5_1 = flush_make [14; 12; 11; 9; 7]

let pocket5_2 = 
  [card_make "Spades" 2; 
   card_make "Hearts" 13]
let best_hand5_2 = flush_make [14; 11; 9; 7; 2]

let table_cards6 = 
  [card_make "Spades" 5;
   card_make "Spades" 6;
   card_make "Spades" 11;
   card_make "Spades" 7;
   card_make "Spades" 12]

let pocket6_1 = 
  [card_make "Spades" 10; 
   card_make "Diamonds" 3]
let best_hand6_1 = flush_make [12; 11; 10; 7; 6]

let pocket6_2 = 
  [card_make "Diamonds" 2; 
   card_make "Hearts" 13]
let best_hand6_2 = flush_make [12; 11; 7; 6; 5]

let best_hand_test 
    (name : string)
    (cards : card list) 
    (pocket : card list)
    (expected_output : hand) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (best_hand cards pocket) 
        ~printer: pp_hand)

let hand_compare_test 
    (name : string)
    (hand1 : hand) 
    (hand2 : hand)
    (expected_output : int) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (hand_compare hand1 hand2) 
        ~printer: string_of_int)

let hand_test
    (name : string)
    (cards : card list) 
    (expected_output : hand) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output (hand_of_cards cards) 
        ~printer: pp_hand)


let hand_tests = [
  high_test "high1: high of 7" high1 high1_hand;
  high_test "high1_equal: high of 7" high1_equal high1_hand_equal;
  high_test "high2: high of 7" high2 high2_hand;
  high_test "high3: high of 10" high3 high3_hand;
  high_test "high4: high of 10" high4 high4_hand;
  pair_test "pair1: pair of 2's" pair1 pair1_hand;
  royalflush_test "royalflush" royalflush royalflush_hand;

  hand_test "hand_test royalflush" royalflush royalflush_hand;

  hand_test "hand_test straightflush" straight_flush straightflush_hand;
  hand_test "hand_test straightflush2" straight_flush2 straightflush_hand2;

  hand_test "hand_test four" four_kind four_hand;
  hand_test "hand_test four" four_kind2 four_hand2;

  hand_test "hand_test fullhouse" full_house fullhouse_hand;
  hand_test "hand_test fullhouse" fuller_house fullerhouse_hand;

  hand_test "hand_test flush" flush flush_hand;
  hand_test "hand_test flush" flush2 flush_hand2;

  hand_test "hand_test straight" straight straight_hand;
  hand_test "hand_test straight2" straight2 straight_hand2;
  hand_test "hand_test straight3, on 14, 13, ..." straight3 straight_hand3;

  hand_test "hand_test three1" three_kind1 three_hand1;
  hand_test "hand_test three2" three_kind2 three_hand2;
  hand_test "hand_test three3" three_kind3 three_hand3;
  hand_test "hand_test three4" three_kind4 three_hand4;

  hand_test "hand_test twopairs1" twopairs1 twopairs1_hand;
  hand_test "hand_test twopairs2" twopairs2 twopairs2_hand;
  hand_test "hand_test twopairs3" twopairs3 twopairs3_hand;

  hand_test "hand_test pair" pair1 pair1_hand;
  hand_test "hand_test high" high1 high1_hand;

  best_hand_test "twopair (6, 5, 12)" table_cards1 pocket1 best_hand1;
  best_hand_test "flush 6" table_cards1 pocket2 best_hand2;
  best_hand_test "straight 7" table_cards1 pocket3 best_hand3;
  best_hand_test "pair 7" table_cards1 pocket4 best_hand4;
  best_hand_test "high 14" table_cards1 pocket5 best_hand5;
  best_hand_test "royal flush" table_cards2 pocket2_1 best_hand2_1;
  best_hand_test "straight flush" table_cards2 pocket2_2 best_hand2_2;
  best_hand_test "straight 14" table_cards2 pocket2_3 best_hand2_3;
  best_hand_test "3 straights, straight 9" table_cards3 pocket3_1 best_hand3_1;
  best_hand_test "3 straights, straight 10" table_cards3 pocket3_2 best_hand3_2;
  best_hand_test "3 straights, straight 8" table_cards3 pocket3_3 best_hand3_3;
  best_hand_test "table flush 12 (none in pocket)" 
    table_cards4 pocket4_1 best_hand4_1;
  best_hand_test "table flush 12 (low in pocket)" 
    table_cards4 pocket4_2 best_hand4_2;
  best_hand_test "flush 13" table_cards4 pocket4_3 best_hand4_3;
  best_hand_test "flush 12 (12 in hand)" table_cards5 pocket5_1 best_hand5_1;
  best_hand_test "flush 2 (2 in hand)" table_cards5 pocket5_2 best_hand5_2;
  best_hand_test "flush w/ one in pocket" table_cards6 pocket6_1 best_hand6_1;
  best_hand_test "flush w/ none in pocket" table_cards6 pocket6_2 best_hand6_2;
  (*compare tests: *)
  hand_compare_test "comparing highs: 1 loses to 2" high1_hand high2_hand ~-1; 
  hand_compare_test "comparing highs: 2 beats 1" high2_hand high1_hand 1; 
  hand_compare_test "comparing highs: tie" high1_hand high1_hand_equal 0; 
  hand_compare_test "comparing highs: 4 beats 3" high4_hand high3_hand 1; 
  hand_compare_test "comparing highs: 3 loses to 4" high3_hand high4_hand ~-1; 

  hand_compare_test "comparing two pairs: 1 ties 2" 
    twopairs1_hand twopairs2_hand 0;
  hand_compare_test "comparing two pairs: 5 beats 4" 
    twopairs5_hand twopairs4_hand 1;
  hand_compare_test "comparing two pairs: 4 beats 3" 
    twopairs4_hand twopairs3_hand 1;
  hand_compare_test "comparing two pairs: 3 loses to 5"
    twopairs3_hand twopairs5_hand ~-1;
  hand_compare_test "comparing two pairs: 5 loses to 1" 
    twopairs5_hand twopairs1_hand ~-1;
]

let is_hand_tests = [
  is_twopair_test "full_house is not twopair" full_house false;
  is_twopair_test "fuller_house is not twopair" fuller_house false;
  is_twopair_test "three_kind1 is not twopair" three_kind1 false;
  is_twopair_test "three_kind2 is not twopair" three_kind2 false;
  is_twopair_test "three_kind3 is not twopair" three_kind3 false;
  is_twopair_test "three_kind4 is not twopair" three_kind4 false;
  is_twopair_test "straight is not twopair" straight false;
  is_twopair_test "flush is not twopair" flush false;

  is_trips_test "trips1" three_kind1 true; 
  is_trips_test "trips2" three_kind2 true; 
  is_trips_test "trips3" three_kind3 true; 
  is_trips_test "twopair1" twopairs1 false;
  is_trips_test "pair1" pair1 false;

  is_straight_test "straight" straight true;
  is_straight_test "straight2" straight2 true;
  is_straight_test "four_kind" four_kind false;

  is_flush_test "flush" flush true;
  is_flush_test "straight" straight false;

  is_straightflush_test "straightflush" straight_flush true;
  is_straightflush_test "straight" straight false;
  is_straightflush_test "flush" flush false;

  is_four_test "four_kind" four_kind true;
  is_four_test "four_kind2" four_kind2 true;
  is_four_test "straight" straight false;
  is_four_test "flush" flush false;
  is_four_test "three_kind1" three_kind1 false;

  is_fullhouse_test "full_house" full_house true;
  is_fullhouse_test "fuller_house" fuller_house true;
  is_fullhouse_test "three_kind1" three_kind1 false;
  is_fullhouse_test "twopairs1" twopairs1 false;
  is_fullhouse_test "four_kind" four_kind false;
]


(**Subject to change:
   Currently runs all existing functionalities in sequence *)
let integration_helper player_array = 
  let hearts = suitMaker [] "Hearts" 2 in
  let diamonds = suitMaker [] "Diamonds" 2 in
  let clubs = suitMaker [] "Clubs" 2 in
  let spades = suitMaker [] "Spades" 2 in
  let std_deck = hearts @ diamonds @ clubs @ spades in 
  let shuffled_deck = shuffle std_deck in
  (deal shuffled_deck player_array 0, 
   shed shuffled_deck 0 (Array.length player_array))

(**Subject to change:
   As of right now, tests that suitMaker, shuffle, and deal in sequence return
   the expected undealt deck *)
let integration_test 
    (name : string)
    (output : card list)
    (expected_output : card list) : test = 
  name >:: (fun _ -> 
      assert_equal expected_output output ~printer:(pp_list pp_card))

let integrated1 = integration_helper players1

let integration_tests = [
  integration_test "integration test" (fst integrated1) (snd integrated1) 
]

let get_tuple_elt (ftr : int * int * int) element  = 
  match ftr with 
  | (f, t, r) -> 
    if element = 1 then f
    else if element = 2 then t 
    else if element = 3 then r
    else failwith "element input invalid"

let pp_tuple tuple = "\"First num: " ^ string_of_int (get_tuple_elt tuple 1) ^
                     ", snd num: " ^ string_of_int (get_tuple_elt tuple 2) 
                     ^ ", thrd num: "  ^ string_of_int (get_tuple_elt tuple 3) 
                     ^ "\""

(* let monte_carlo_test
    (name : string)
    (pocket : card list)
    (table : card list)
    (num_players : int)
    (expected_output : int * int * int) : test = 
   name >:: (fun _ -> 
      assert_equal expected_output (monte_carlo pocket table num_players) 
      ~printer:(pp_tuple)) *)

(* let pocket_rank_testerA = 
   [card_make "Spades" 8; 
   card_make "Hearts" 7] *)

(* let pocket_rank_testerD = 
   [card_make "Clubs" 2; 
   card_make "Spades" 4]

   let pocket_rank_testerH = 
   [card_make "Clubs" 4; 
   card_make "Hearts" 9]

   let pocket_rank_testerC = 
   [card_make "Hearts" 8; 
   card_make "Hearts" 2]

   let pocket_rank_testerB = 
   [card_make "Hearts" 14; 
   card_make "Diamonds" 14]

   let ai_tests = [
   monte_carlo_test "pocket aces" pocket_rank_testerB [] 4 (1000, 600, 250);
   monte_carlo_test "8 and 7, diff suit" 
    pocket_rank_testerA [] 4 (1000, 600, 250);
   monte_carlo_test "2 and 4, diff suit" 
    pocket_rank_testerD [] 4 (1000, 600, 250);
   monte_carlo_test "4 and 9, diff suit" 
    pocket_rank_testerH [] 4 (1000, 600, 250);
   monte_carlo_test "2 and 8, same suit" 
    pocket_rank_testerC [] 4 (1000, 600, 250); 
   ]*)

let tests =
  "test suite for A1"  >::: List.flatten [
    deal_tests;
    shuffle_tests;
    integration_tests;
    bet_tests;
    round_end_tests;
    hand_tests;
    is_hand_tests;
    (* ai_tests; *)
  ]

let _ = run_test_tt_main tests
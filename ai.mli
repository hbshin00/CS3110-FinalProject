open Cards 
open State 

(** [monte_carlo pocket table numplayers] =
    [pocket]: the card list of the player's 2 cards in hand
    [table]: the card list of cards visible to the player on the table
    [numplayers]: the number of players being tested against
    Returns: a tuple of 1000, number of wins out of 1000 iterations,
    number of tie wins out of 1000 iterations.
    This is meant to give an approximation of hand strength
    Precondition: table can be of length 0, 3, 4, 5*)
val monte_carlo : card list -> card list -> int -> int * int * int


(**[odds str mc_tuple] returns a float (percentage) of wins, ties, or both as 
   specified by [str]. The str "both_w" returns the percentage of both wins and
   ties with ties given 0.5 weight. Fails if [str] is not a valid specifier.*)
val odds : string -> int * int * int -> float


(**[pocket_odds str mc_tuple] returns a float (percentage) of wins, ties, or 
   both as specified by [str]. The str "both_w" returns the percentage of both 
   wins and ties with ties given 0.5 weight. Fails if [str] is not a valid 
   specifier. The values returned correspond to the odds during the pocket 
   round, before any of the table cards have been revealed. *)
val pocket_odds : string -> float -> int * int


(**[pocket_odds str mc_tuple] returns a float (percentage) of wins, ties, or 
   both as specified by [str]. The str "both_w" returns the percentage of both 
   wins and ties with ties given 0.5 weight. Fails if [str] is not a valid 
   specifier. The values returned correspond to the odds during the flop round 
   of betting *)
val flop_odds : string -> float -> int * int


(**[pocket_odds str mc_tuple] returns a float (percentage) of wins, ties, or 
   both as specified by [str]. The str "both_w" returns the percentage of both 
   wins and ties with ties given 0.5 weight. Fails if [str] is not a valid 
   specifier. The values returned correspond to the odds during the turn round 
   of betting *)
val turn_odds : string -> float -> int * int


(**[pocket_odds str mc_tuple] returns a float (percentage) of wins, ties, or 
   both as specified by [str]. The str "both_w" returns the percentage of both 
   wins and ties with ties given 0.5 weight. Fails if [str] is not a valid 
   specifier. The values returned correspond to the odds during the river round 
   of betting. *)
val river_odds : string -> float -> int * int


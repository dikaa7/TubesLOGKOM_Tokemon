/* Move */
n :-
(player_location(X,Y), map_size(Min,Max), Z is Y - 1, Z =< Min, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is Y - 1, Z > Min, retractall(player_location(X,Y)), assertz(player_location(X,Z)), print_location, !).

/* e : menggerakkan pemain satu petak ke arah timur */
e :-
(player_location(X,Y), map_size(Min,Max), Z is X + 1, Z >= Max, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is X + 1, Z < Max, retractall(player_location(X,Y)), assertz(player_location(Z,Y)), print_location, !).

/* s : menggerakkan pemain satu petak ke arah selatan */
s :-
(player_location(X,Y), map_size(Min,Max), Z is Y + 1, Z >= Max, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is Y + 1, Z < Max, retractall(player_location(X,Y)), assertz(player_location(X,Z)), print_location, !).

/* w : menggerakkan pemain satu petak ke arah barat */
w :-
(player_location(X,Y), map_size(Min,Max), Z is X - 1, Z =< Min, write(' Invalid move'));
(player_location(X,Y), map_size(Min,Max), Z is X - 1, Z > Min, retractall(player_location(X,Y)), assertz(player_location(Z,Y)), print_location).


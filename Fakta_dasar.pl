/*
Nama/Nim
1. Ade Surya Handika/13518007
2. Vincent Tanjaya/13518133
3. Made Prisha/13518049
4. Fakhrurrida Widodo/13518091
*/

/* TOKEMON HEALTH FACTS */
health(dragonite,101).
health(firara,62).
health(burster,73).
health(bulbaur,65).
health(oddi,56).
health(exegg,78).
health(enax, 55).
health(alon, 55).
health(segirock, 120).
health(rain,80).
health(octomon,65).
health(dragostorm,50).

/* TOKEMON TYPE FACTS */
type(enax, rock).
type(alon, rock).
type(segirock, rock).
type(dragonite,fire)
type(firara,fire)
type(burster,fire)
type(bulbaur,grass)
type(oddi,grass)
type(exegg,grass)
type(rain,water).
type(octomon,water).
type(dragostorm,water).

/* TOKEMON NORMAL ATTACK FACTS */
normalAttack(enax, 12).
normalAttack(alon, 15).
normalAttack(segirock, 35).
normalAttack(dragonite,38)
normalAttack(firara,17)
normalAttack(burster,13)
normalAttack(bulbaur,17)
normalAttack(oddi,19)
normalAttack(exegg,12)
normalAttack(rain,10).
normalAttack(octomon,15).
normalAttack(dragostorm,20).

/* TOKEMON SPECIAL ATTACK FACTS */
specialAttack(dragonite,59).
specialAttack(firara,25).
specialAttack(burster,23).
specialAttack(bulbaur,25).
specialAttack(oddi,26).
specialAttack(exegg,30).
specialAttack(enax, 25).
specialAttack(alon, 22).
specialAttack(segirock, 55).
specialAttack(rain,20).
specialAttack(octomon,25).
specialAttack(dragostorm,30).

/* TOKEMON RARITY FACTS */
rarity(dragonite,legendary).
rarity(firara,normal).
rarity(burster,normal).
rarity(bulbaur,normal).
rarity(oddi,normal).
rarity(exegg,normal).
rarity(enax,normal).
rarity(alon,normal).
rarity(segirock,legendary).
rarity(rain,normal).
rarity(octomon,normal).
rarity(dragostorm,normal).

/* TOKEMON MAP FACTS */
/*G adalah Gym Center dan X adalah pembatas*/
coordinate(X, Y, Symbol) :- X == 5, Y == 4, Symbol = G, !.
coordinate(X, Y, Symbol) :- X == 14, Y == 12, Symbol = G, !.
coordinate(X, Y, Symbol) :- X == 1, Symbol = X, !.
coordinate(X, Y, Symbol) :- X == 22, Symbol = X, !.
coordinate(X, Y, Symbol) :- Y == 1, Symbol = X, !.
coordinate(X, Y, Symbol) :- Y == 22, Symbol = X.


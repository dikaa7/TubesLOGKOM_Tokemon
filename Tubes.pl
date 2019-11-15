
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
type(dragonite,fire).
type(firara,fire).
type(burster,fire).
type(bulbaur,grass).
type(oddi,grass).
type(exegg,grass).
type(rain,water).
type(octomon,water).
type(dragostorm,water).

/* TOKEMON NORMAL ATTACK FACTS */
normalAttack(enax, 12).
normalAttack(alon, 15).
normalAttack(segirock, 35).
normalAttack(dragonite,38).
normalAttack(firara,17).
normalAttack(burster,13).
normalAttack(bulbaur,17).
normalAttack(oddi,19).
normalAttack(exegg,12).
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
map_size(0,21).



help :-	write('Available commands:'),nl,
write('   start. --start the game!'),nl,
write('   help. --show available commands'),nl,
write('   quit. --quit the game'),nl,
write('   n. s. e. w. --move'),nl,
write('   map. --look at the map'),nl,
write('   heal --cure Tokemon in inventory if in gym center'),nl,
write('   status. --show your status'),nl,
write('   save(Filename). --save your game'),nl,
write('   load(Filename). --load previously saved game'),nl,
write('Legends : '),nl,
write('- X = Pagar'),nl,
write('- P = Player'),nl,
write('- G = Gym'),nl.
			
/* Variabel Dinamik */
:- dynamic(player_location/2).
:- dynamic(player_tokemon/1).
:- dynamic(tokemon_health/2).
:- dynamic(legend_count/2).

/* Fact Dinamik */
dynamic_facts :-
retractall(player_location(_X,_Y)),
retractall(player_tokemon(_X)),
retractall(tokemon_health(_X,_Y)),
retractall(legend_count(_X)).

init_game:- asserta(player_location(1,1)),
			asserta(gym_pos1(5,4)),
			asserta(gym_pos2(14,12)).
			/*
			random(1,12,X),
			id(Y,X),
			asserta(list_pokemon(Y,health(Y),normalAttack(Y),specialAttack(Y))).
			*/
		
start:- write('   ██████╗  ██████╗ ████████╗████████╗ █████╗     '),nl,                                               
		write(' ██╔════╝ ██╔═══██╗╚══██╔══╝╚══██╔══╝██╔══██╗   '),nl,                                                
		write(' ██║  ███╗██║   ██║   ██║      ██║   ███████║    '),nl,                                               
		write(' ██║   ██║██║   ██║   ██║      ██║   ██╔══██║       '),nl,                                            
		write(' ╚██████╔╝╚██████╔╝   ██║      ██║   ██║  ██║          '),nl,                                         
		write('  ╚═════╝  ╚═════╝    ╚═╝      ╚═╝   ╚═╝  ╚═╝             '),nl,                                      
																										   
		write(' ██████╗ █████╗ ████████╗ ██████╗██╗  ██╗    ███████╗███╗   ███╗     █████╗ ██╗     ██╗     ██╗'),nl,
		write('██╔════╝██╔══██╗╚══██╔══╝██╔════╝██║  ██║    ██╔════╝████╗ ████║    ██╔══██╗██║     ██║     ██║'),nl,
		write('██║     ███████║   ██║   ██║     ███████║    █████╗  ██╔████╔██║    ███████║██║     ██║     ██║'),nl,
		write('██║     ██╔══██║   ██║   ██║     ██╔══██║    ██╔══╝  ██║╚██╔╝██║    ██╔══██║██║     ██║     ╚═╝'),nl,
		write('╚██████╗██║  ██║   ██║   ╚██████╗██║  ██║    ███████╗██║ ╚═╝ ██║    ██║  ██║███████╗███████╗██╗'),nl,
		write(' ╚═════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝╚═╝  ╚═╝    ╚══════╝╚═╝     ╚═╝    ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝'),nl,
		write("Gotta catch 'em all!"),

		write('Hi there! Welcome to the world of Tokemon! My name is Tome!                              '),nl,
		write('There are a lot of tokemon all around you! Catch them to get stronger! '),nl,
		write('But your final mission is to defeat or capture all those legendary tokemon, if you fail, '),nl,
		write('you will be out from this world.            '),
		help,
		init_game.
map:- printmap(0,0),!.
printmap(X,Y):- player_location(Xa,Ya),
				X == Xa, Y == Ya,
				write('P'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- gym_pos1(Xa,Ya),
				X==Xa, Y==Ya,
				write('G'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- gym_pos2(Xa,Ya),
				X==Xa,Y==Ya,
				write('G'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- X == 22, Y<22,
				write(""),nl,
				M is 0,!,
				N is Y+1,
				printmap(M,N),!.
printmap(X,Y):- X==0, Y<22,
				write('X'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- Y==0,X<22,
				write('X'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- Y==21,X<22,
				write('X'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- X==21,X<22,
				write('X'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- X<22,Y<22,
				write('-'),
				M is X+1,
				N is Y,!,
				printmap(M,N),!.
printmap(X,Y):- X>21,Y>21,!.
/* Move */
n :-
(player_location(X,Y), map_size(Min,Max), Z is Y - 1, Z =< Min, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is Y - 1, Z > Min, retractall(player_location(X,Y)), assertz(player_location(X,Z)), !).

/* e : menggerakkan pemain satu petak ke arah timur */
e :-
(player_location(X,Y), map_size(Min,Max), Z is X + 1, Z >= Max, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is X + 1, Z < Max, retractall(player_location(X,Y)), assertz(player_location(Z,Y)), !).

/* s : menggerakkan pemain satu petak ke arah selatan */
s :-
(player_location(X,Y), map_size(Min,Max), Z is Y + 1, Z >= Max, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is Y + 1, Z < Max, retractall(player_location(X,Y)), assertz(player_location(X,Z)), !).

/* w : menggerakkan pemain satu petak ke arah barat */
w :-
(player_location(X,Y), map_size(Min,Max), Z is X - 1, Z =< Min, write(' Invalid move'));
(player_location(X,Y), map_size(Min,Max), Z is X - 1, Z > Min, retractall(player_location(X,Y)), assertz(player_location(Z,Y)),!).

						

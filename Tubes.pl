
/*
Nama / NIM
1. Ade Surya Handika / 13518007
2. Vincent Tanjaya / 13518133
3. Made Prisha / 13518049
4. Fakhrurrida Clarendia Widodo / 13518091
*/

/* START MESSAGE */
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
		init_game,
		initChar.

/* TOKEMON HEALTH FACTS */
health(1,101).
health(2,62).
health(3,73).
health(4,65).
health(5,56).
health(6,78).
health(7,55).
health(8,55).
health(9,120).
health(10,80).
health(11,65).
health(12,50).

/* TOKEMON MAX HEALTH FACTS */
maxhealth(1,101).
maxhealth(2,62).
maxhealth(3,73).
maxhealth(4,65).
maxhealth(5,56).
maxhealth(6,78).
maxhealth(7,55).
maxhealth(8,55).
maxhealth(9,120).
maxhealth(10,80).
maxhealth(11,65).
maxhealth(12,50).

/* TOKEMON TYPE FACTS */
type(enax,rock).
type(alon,rock).
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

id(dragonite,1).
id(firara,2).
id(burster,3).
id(bulbaur,4).
id(oddi,5).
id(exegg,6).
id(enax,7).
id(alon,8).
id(segirock,9).
id(rain,10).
id(octomon,11).
id(dragostorm,12).

map_size(0,21).

test:- rarity(X,Y), X==dragonite, write(Y).

/* HELP DESK */
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
:- dynamic(legend_count/2).
:- dynamic(playerStatus/2).

/* Fact Dinamik */
dynamic_facts :-
retractall(player_location(_X,_Y)),
retractall(player_tokemon(_X)),
retractall(tokemon_health(_X,_Y)),
retractall(legend_count(_X)).

/* INITIATE ATTRIBUTE OF CHARACTER */
initNbToke(1).

randomFirstTokemon(RandomToke) :-

random(1, 12, Nr), 
Nr\==1,
Nr\==9,
id(Toke, Nr),
RandomToke is Toke.

initTokeList([]).

/* INITIATE CHARACTER STATUS */
initChar:-
    initNbToke(NbToke),
    initTokeList(TokeList),
    randomFirstTokemon(Random),
    append([Random], TokeList, NewTokeList),
    asserta(playerStatus(NewTokeList, NbToke)), !.

/* INITIATE GAME */
init_game:- asserta(player_location(1,1)),
			asserta(gym_pos1(5,4)),
			asserta(gym_pos2(14,12)).
			/*
			random(1,12,X),
			id(Y,X),
			asserta(tokemon_status(Y,health(Y),normalAttack(Y),specialAttack(Y))),
			initChar.
			*/
		
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

/* MOVE */
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

/* ADDED ON 17/11/2019 */
/* CEK DI GYM */
dalamGym :-
(player_location(X,Y), gym_pos1(Xa, Ya), X==Xa, X==Ya,!);
(player_location(X,Y), gym_pos2(Xb, Yb), X==Xb, X==Yb,!).

/* MELAKUKAN RETREAT PADA TOKEMON */
retreat(Tokemon) :-
(health(Tokemon, Current), maxhealth(Tokemon, Max), Current is Max).

/* HEALING PROCESS
heal :-
dalamGym,
playerStatus(Inventory, NbToke),

retreat */
status:- playerStatus(TokemonList1, NbTokemon),
		write('Your Tokemon:'),nl,
		printstatus(TokemonList1, NbTokemon),!.
printstatus(TokemonL,NbTokemon):- NbTokemon == 0,
								nl,write('Your Enemy : '),nl,
									id(TokemonName,1),
									write(TokemonName),nl,
									health(1,Health),
									write('Health:'),write(Health),nl,
									id(TokemonName1,9),
									write(TokemonName1),nl,
									health(9,Health2),
									write('Health:'),write(Health2),nl.
printstatus(TokemonL,NbTokemon):- NbTokemon > 0,
								[Head|T] = TokemonL,
								 id(TokemonName,Head),
								 write(TokemonName),nl,
								 health(Head,Health1),
								 write('Health :'),write(Health1),nl,
								 NbToke is NbTokemon-1,
								 printstatus(T,NbToke).
			
/* ADD CAPTURED TOKEMON TO INVENTORY */
addTokemon(CapturedT) :-
retract(playerStatus(TokemonList, NbTokemon)),
append([CapturedT],TokemonList, NewTokeList),
NewNbToke is NbTokemon + 1,
asserta(playerStatus(NewTokeList, NewNbToke)).

/* ERASE LOST TOKEMON FROM INVENTORY */
eraseTokemon(DeadT) :-
retract(playerStatus(TokemonList, NbTokemon)),
delete_one(CapturedT, TokemonList, NewTokeList),
NewNbToke is NbTokemon - 1,
asserta(playerStatus(NewTokeList, NewNbToke)).

delete_one(_, [], []).
delete_one(Term, [Term|Tail], Tail) :- !.
delete_one(Term, [Head|Tail], [Head|Result]) :-
    delete_one(Term, Tail, Result).

 :- dynamic(playerStatus/2).

/* SAVE */

save_game :-
nl, write('Name of your file : '),
nl, read(File),
atom_concat(File, '.txt', Filetext),
open(Filetext, write, Stream),
save_all(Stream),
close(Stream),
write('Saved !'), nl.

save_all(Stream) :-
save_playerStatus(Stream).

save_all(Stream) :-
save_player_location(Stream).

save_all(Stream) :-
save_player_tokemon(Stream).

save_all(Stream) :-
save_tokemon_health(Stream).

save_all(Stream) :-
save_legend_count(Stream).

save_playerStatus(Stream) :-
playerStatus(TokemonList,NbTokemon),
write(Stream, playerStatus(TokemonList,NbTokemon)),
write(Stream, '.'),
nl(Stream),
fail.

save_player_location(Stream) :-
player_location(X,Y),
write(Stream,player_location(X,Y)),
write(Stream, '.'),
nl(Stream),
fail.

save_player_tokemon(Stream) :-
player_tokemon(X),
write(Stream, player_tokemon(X)),
write(Stream,'.'),
nl(Stream),
fail.

save_tokemon_health(Stream) :-
tokemon_health(X,Y),
write(Stream, tokemon_health(X,Y)),
write(Stream, '.'),
nl(Stream),
fail.

save_legend_count(Stream) :-
legend_count(X),
write(Stream, legend_count(X)),
write(Stream, '.'),
nl(Stream),
fail.

/* LOAD */

load_game :-
nl, write('Name of your file : '),
nl, read(File),
atom_concat(File, '.txt', Filetext),
load_all(Filetext).

load_all(Filetext) :-
retractall(playerStatus(_,_)),
retractall(player_location(_,_)),
retractall(player_tokemon(_)),
retractall(tokemon_health(_,_)),
retractall(legend_count(_)),
open(Filetext, read, Stream),
repeat,
read(Stream, In),
asserta(In),
at_end_of_stream(Stream),
close(Stream,
nl, write('Loaded !')), nl, !.

load_all(_) :-
nl, write('Wrong input !'), nl, fail.

/* NYEBAR POKEMON */
randomTokemon :-
    repeat,
    random(1, 12, A), id(Z, A),
    random(0, 19, X), random(0, 19, Y),
    gym_pos1(Xa,Ya), gym_pos2(Xb, Yb),
    X\==Xa,
    X\==Xb,
    Y\==Ya,
    Y\==Yb,
    asserta(tokemon_pos(X,Y,Z)).

spread_tokemon(0) :-!.
spread_tokemon(B) :-
    randomTokemon,
    C is B-1,
    init_weapon(C).

 init_game:- asserta(player_location(1,1)),
			asserta(gym_pos1(5,4)),
			asserta(gym_pos2(14,12)).


/*
Nama / NIM
1. Ade Surya Handika / 13518007
2. Vincent Tanjaya / 13518133
3. Made Prisha / 13518049
4. Fakhrurrida Clarendia Widodo / 13518091
*/

/* START MESSAGE */
start:- /*write('   ██████╗  ██████╗ ████████╗████████╗ █████╗     '),nl,                                               
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
		write(' ╚═════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝╚═╝  ╚═╝    ╚══════╝╚═╝     ╚═╝    ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝'),nl,*/
		nl,
		write('Gotta catch `em all!'),nl,nl,

		write('Hi there! Welcome to the world of Tokemon! My name is Tome!                              '),nl,
		write('There are a lot of tokemon all around you! Catch them to get stronger! '),nl,
		write('But your final mission is to defeat or capture all those legendary tokemon, if you fail, '),nl,
		write('you will be out from this world.            '),nl,nl,
		help,
		init_game,
		initChar.

/* TOKEMON HEALTH FACTS */


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

/* TOKEMON ID */
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

/* MAP SIZE FACT */
map_size(0,21).

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
			
/* DYNAMIC VARIABLES */
:- dynamic(player_location/2).
:- dynamic(player_tokemon/1).
:- dynamic(banyak/1).
:- dynamic(health/2).
:- dynamic(position/3).
:- dynamic(legend_count/1).
:- dynamic(playerStatus/2).

/* DYNAMIC FACTS */
dynamic_facts :-
retractall(player_location(_X,_Y)),
retractall(player_tokemon(_X)),
retractall(tokemon_health(_X,_Y)),
retractall(legend_count(_X)).
banyak(1).
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

position(1,20,20).
position(2,4,5).
position(3,12,9).
position(4,0,0).
position(5,18,3).
position(6,2,14).
position(7,8,19).
position(8,7,13).
position(9,20,1).
position(10,14,3).
position(11,3,2).
position(12,4,1).

/* INITIATE ATTRIBUTE OF CHARACTER */
initNbToke(1).

randomFirstTokemon(RandomToke) :-
random(1, 12, Nr),
Nr\=9,
Nr\=1,
RandomToke is Nr.

initTokeList([]).

/* INITIATE CHARACTER STATUS */
initChar:-
    initNbToke(NbToke),
    initTokeList(TokeList),
    append([4], TokeList, NewTokeList),
    asserta(playerStatus(NewTokeList, NbToke)), !.

/* INITIATE GAME */
init_game:- asserta(player_location(1,1)),
			asserta(gym_pos1(5,4)),
			asserta(gym_pos2(14,12)).
		
/* MAP. COMMAND */		
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
				write(''),nl,
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
(player_location(X,Y), map_size(Min,Max), Z is Y - 1, Z > Min, retractall(player_location(X,Y)), assertz(player_location(X,Z)), !),cek(X,Z,1).

/* e : menggerakkan pemain satu petak ke arah timur */
e :-
(player_location(X,Y), map_size(Min,Max), Z is X + 1, Z >= Max, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is X + 1, Z < Max, retractall(player_location(X,Y)), assertz(player_location(Z,Y)), !),cek(Z,Y,1).

/* s : menggerakkan pemain satu petak ke arah selatan */
s :-
(player_location(X,Y), map_size(Min,Max), Z is Y + 1, Z >= Max, write(' Invalid move'), !);
(player_location(X,Y), map_size(Min,Max), Z is Y + 1, Z < Max, retractall(player_location(X,Y)), assertz(player_location(X,Z)), !),cek(X,Z,1).

/* w : menggerakkan pemain satu petak ke arah barat */
w :-
(player_location(X,Y), map_size(Min,Max), Z is X - 1, Z =< Min, write(' Invalid move'));
(player_location(X,Y), map_size(Min,Max), Z is X - 1, Z > Min, retractall(player_location(X,Y)), assertz(player_location(Z,Y)),!),cek(Z,Y,1).

/* MENGECEK POSISI KARAKTER DALAM MAP */
cek(X,Y,Z):- gym_pos1(Xa,Ya),
		   X == Xa, Y == Ya,
		   write('Anda sedang berada di gym, anda dapat menggunakan command heal untuk menyembuhkan tokemon anda'),nl,!.
cek(X,Y,Z):- gym_pos2(Xa,Ya),
			X == Xa, Y == Ya,
			write('Anda sedang berada di gym, anda dapat menggunakan command heal untuk menyembuhkan tokemon anda'),nl,!.
cek(X,Y,Z):- Z<13,
			position(Z,Xa,Ya),
			X == Xa, Y == Ya,
			write('A wild Tokemon appears'),nl,
			write('Fight or Run?'),nl,!.
cek(X,Y,Z):- Z<13,
			Za is Z+1,
			cek(X,Y,Za),!.

/* CEK DI GYM */
dalamGym :-
(player_location(X,Y), gym_pos1(Xa, Ya), X==Xa, X==Ya,!);
(player_location(X,Y), gym_pos2(Xb, Yb), X==Xb, X==Yb,!).

/* MELAKUKAN RETREAT PADA TOKEMON */
retreat(Tokemon) :-
(id(TokemonName,Tokemon),tokemon(TokemonName,Health1,Ta,Tb,Tc,Td), maxhealth(Tokemon, Max),retract(tokemon(TokemonName,Health1,Ta,Tb,Tc,Td)),asserta(tokemon(TokemonName,Max,Ta,Tb,Tc,Td))).

heal :- playerStatus(TokemonL,NbToke),retract(banyak(Ta)),asserta(banyak(0)),
		heal1(TokemonL,NbToke).
heal1(TokemonL,NbToke):- NbToke>0,
						[H|T] = TokemonL,
						retreat(H),NbToke1 is NbToke-1,
						heal1(T,NbToke1).

status:- playerStatus(TokemonList1, NbTokemon),
		write('Your Tokemon:'),nl,
		printstatus(TokemonList1, NbTokemon),!.
printstatus(TokemonL,NbTokemon):- NbTokemon == 0,
								nl,write('Your Enemy : '),nl,nl,
									id(TokemonName,1),
									write(TokemonName),nl,
									tokemon(TokemonName,Health1,_,_,_,_),
									write('Health:'),write(Health1),nl,
									id(TokemonName1,9),
									write(TokemonName1),nl,
									tokemon(TokemonName1,Health2,_,_,_,_),
									write('Health:'),write(Health2),nl.
printstatus(TokemonL,NbTokemon):- NbTokemon > 0,
								[Head|T] = TokemonL,
								 id(TokemonName,Head),
								 write(TokemonName),nl,
								 tokemon(TokemonName,Health1,_,_,_,_),
								 write('Health :'),write(Health1),nl,
								 NbToke is NbTokemon-1,
								 printstatus(T,NbToke).
			
/* ADD CAPTURED TOKEMON TO INVENTORY */
/* inspirasi: https://github.com/pandyakaa/Prolog-BattleRoyale */

addTokemon(CapturedT) :-
retract(playerStatus(TokemonList, NbTokemon)),
append([CapturedT],TokemonList, NewTokeList),
NewNbToke is NbTokemon + 1,
asserta(playerStatus(NewTokeList, NewNbToke)).

/* ERASE LOST TOKEMON FROM INVENTORY */
/* inspirasi: https://github.com/pandyakaa/Prolog-BattleRoyale */

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

drop(X) :- id(Id,X),
        eraseTokemon(Id),!.

/* MENYIMPAN FILE KONFIGURASI */
/* inspirasi: https://github.com/pandyakaa/Prolog-BattleRoyale */
save :-
			nl, write('Masukkan nama file untuk menyimpan game-mu!'), nl,
			write('% '), 
			read(File),
			atom_concat(File, '.txt', Filetxt),
			open(Filetxt, write, Ekstern),
			savecfg(Ekstern),
			close(Ekstern),
			write('Game-mu telah berhasil disimpan!'), nl.


savecfg(Ekstern) :-
				saveposition(Ekstern).
savecfg(Ekstern) :-
				saveNbLegend(Ekstern).
savecfg(Ekstern) :-
				saveTokeStats(Ekstern).
savecfg(Ekstern) :-
				savePlStatus(Ekstern).
savecfg(Ekstern) :-
				saveTokePosition(Ekstern).
savecfg(_) :- !.


saveposition(Ekstern) :-
			 	player_location(X,Y),
				write(Ekstern, player_location(X,Y)), 
				write(Ekstern, '.'), 
				nl(Ekstern),
				fail.

saveNbLegend(Ekstern) :-
				legend_count(X),
				write(Ekstern, legend_count(X)), 
				write(Ekstern, '.'), 
				nl(Ekstern),
				fail.
saveTokeStats(Ekstern) :-
				tokemon(A,B,C,D,E,F),
				write(Ekstern, tokemon(A,B,C,D,E,F)),
				write(Ekstern, '.'),
				nl(Ekstern),
				fail.
savePlStatus(Ekstern) :-
			 	playerStatus(X,Y),
				write(Ekstern, playerStatus(X,Y)), 
				write(Ekstern, '.'), 
				nl(Ekstern),
				fail.
saveTokePosition(Ekstern) :-
			 	position(G,H,I),
				write(Ekstern, position(G,H,I)), 
				write(Ekstern, '.'), 
				nl(Ekstern),
				fail.

/* MEMUAT FILE KONFIGURASI */
/* inspirasi: https://github.com/pandyakaa/Prolog-BattleRoyale */
load :-
				nl, 
				write('Masukkan nama file yang akan dimuat!') , nl,
				write('% '), 
				read(File),
				atom_concat(File,'.txt',Filetxt),
				loadcfg(Filetxt).

loadcfg(Filetxt) :-
				retractall(player_location(_,_)),
				retractall(legend_count(_)),
				retractall(tokemon(_,_,_,_,_,_)),
				retractall(playerStatus(_,_)),
				retractall(position(_,_,_)),
				open(Filetxt, read, Ekstern),
				repeat,
						read(Ekstern, In),
						asserta(In),
				at_end_of_stream(Ekstern),
				close(Ekstern),
				nl, 
				write('File berhasil dimuat. Selamat bermain Tokemon kembali!'), nl, !.

loadcfg(_):-
			  nl, 
			  write('File salah! Silakan coba ``load.`` kembali!'), nl, 
			  fail.


/* File untuk saat tokemon bertarung */

:- dynamic(enemy/5).
:- dynamic(tokemon/6).
:- dynamic(chosentokemon/2).
:- dynamic(loseCondition/0).
:- dynamic(inbattle/1).
:- dynamic(legend_count/1).

legend_count(0).

/* tokemon(nama,health,normalAttack,specialAttack,type,rarity). */
tokemon(dragonite,101,38,59,fire,legendary).
tokemon(firara,62,17,25,fire,normal).
tokemon(burster,73,13,23,fire,normal).
tokemon(bulbaur,65,17,25,grass,normal).
tokemon(oddi,56,19,26,grass,normal).
tokemon(exegg,78,12,30,grass,normal).
tokemon(enax,55,12,25,rock,normal).
tokemon(alon,55,15,22,rock,normal).
tokemon(segirock,120,35,55,rock,legendary).
tokemon(rain,80,10,20,water,normal).
tokemon(octomon,65,15,25,water,normal).
tokemon(dragostorm,50,20,30,water,normal).
/*
enemy(dragonite,101,38,59,fire).
enemy(firara,62,17,25,fire).
enemy(burster,73,13,23,fire).
enemy(bulbaur,65,17,25,grass).
enemy(oddi,56,19,26,grass).
enemy(exegg,78,12,30,grass).
enemy(enax,55,12,25,rock).
enemy(alon,55,15,22,rock).
enemy(segirock,120,35,55,rock).
enemy(rain,80,10,20,water).
enemy(octomon,65,15,25,water).
enemy(dragostorm,50,20,30,water).
*/
/* Strong Type Tokemon */
strong(fire,grass).
strong(grass,water).
strong(water,fire).
strong(rock,fire).

strong(water,rock).

inbattle(0).

/* State saat kalah dan menang */
/* loseCondition :- %kondisi kalah
# winCondition :- %kondisi menang*/
win :- write('Kamu menang ... :) '),nl,!.
lose :- write('Kamu kalah.. :P'),nl,!.


fight :- asserta(inbattle(1)),player_location(Xa,Ya),position(Z,Xa,Ya),Z == 1,retract(legend_count(Ta)),Tb is Ta+1,asserta(legend_count(Tb)),id(X,Z),tokemon(X,Ta,Tb,Tc,Td,Te),asserta(enemy(X,Ta,Tb,Tc,Td)).
fight :- asserta(inbattle(1)),player_location(Xa,Ya),position(Z,Xa,Ya),Z == 9,retract(legend_count(Ta)),Tb is Ta+1,asserta(legend_count(Tb)),id(X,Z),tokemon(X,Ta,Tb,Tc,Td,Te),asserta(enemy(X,Ta,Tb,Tc,Td)).
fight :- asserta(inbattle(1)),player_location(Xa,Ya),position(Z,Xa,Ya),id(X,Z),tokemon(X,Ta,Tb,Tc,Td,Te),asserta(enemy(X,Ta,Tb,Tc,Td)).

/* PEMILIHAN TOKEMON */
choose1(Id,TokemonList,NbTokemon) :- [H|T] = TokemonList,
                                H == Id, id(X,Id),choose2(X),!.
choose1(Id,TokemonList,NbTokemon) :-  NbTokemon>0,
                                [H|T] = TokemonList,
                                H \= Id, choose1(Id,T,NbTokemon-1).
choose1(Id,TokemonList,NbTokemon) :- NbTokemon == 0,
                                write('Anda tidak memiliki tokemon tersebut'),nl,!.

choose(X) :- id(X,Id),playerStatus(TokemonL,NbToke),
			choose1(Id,TokemonL,NbToke).
choose2(X) :- 
        inbattle(Ta),Ta == 1,
        tokemon(X,_,_,_,_,_), asserta(chosentokemon(X,1)),
        write('Keluarlah,"'),write(X),write('"'),nl,nl, life, !.
choose2(_) :- 
        inbattle(1), 
        chosentokemon(X,_),
        write('Kamu tidak bisa memilih ulang saat bertarung, harap gunakan "change(X)."'),!.
choose2(_) :- 
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung'),nl,!.

attack :- 
        loseCondition, lose, !.
attack :-
        inbattle(T),T==2,
        write('Tokemonmu sudah mati!'), nl,!.
attack :- 
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung'),nl,!.
attack :- 
        inbattle(1), 
        chosentokemon(X,_), tokemon(X,_,Att,_,TypeM,_), enemy(A,HP,B,C,TypeL),
        strong(TypeM, TypeL), D is div(Att * 3, 2), Z is HP - D,
        write('Serangannya sangat efektif!'), nl,
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL)), cekhealthL, !.
attack :- 
        inbattle(1),
        chosentokemon(X,_), tokemon(X,_,Att,_,TypeM,_), enemy(A,HP,B,C,TypeL),
        strong(TypeL, TypeM), D is div(Att, 2), Z is HP - D, 
        write('Serangannya tidak efektif!'), nl, 
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,
        retract(enemy(_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL)), cekhealthL, !.   
attack :- 
        inbattle(1),
        chosentokemon(X,_), tokemon(X,_,Att,_,_,_), enemy(A,HP,B,C,TypeL),
        Z is (HP - Att),
        write('Kamu menyebabkan '), write(Att), write(' damage pada '), write(A),nl,nl,
        retract(enemy(_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL)), cekhealthL, !.

specialAttack :- 
        loseCondition, lose, !.
specialAttack :-
        inbattle(2),
        write('Tokemonnya sudah pingsan!'), nl,!.
specialAttack :-
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung'),nl,!.
specialAttack :- 
        inbattle(1), 
        chosentokemon(X,1), tokemon(X,_,_,Special,TypeM,_), enemy(A,HP,B,C,TypeL),
        strong(TypeM, TypeL), D is div(Special * 3, 2), Z is HP - D,
        write('Serangannya sangat efektif!'), nl,
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL)),
        retract(chosentokemon(X,1)), asserta(chosentokemon(X,0)), cekhealthL, !.              
specialAttack :- 
        inbattle(1), 
        chosentokemon(X,1), tokemon(X,_,_,Special,TypeM,_), enemy(A,HP,B,C,TypeL),
        strong(TypeL, TypeM), D is div(Special, 2), Z is HP - D,
        write('Serangannya tidak efektif!'), nl, 
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)),
        retract(chosentokemon(X,1)), asserta(chosentokemon(X,0)), cekhealthL, !.
specialAttack :-  
        inbattle(1),
        chosentokemon(X,1), tokemon(X,_,_,Special,_,_), enemy(A,HP,B,C,TypeL),
        Z is (HP - Special),
        write('Kamu menyebabkan '), write(Special), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL)),
        retract(chosentokemon(X,1)), asserta(chosentokemon(X,0)), cekhealthL, !.
specialAttack :-  
        chosentokemon(X,N), N =< 1, write(X), write(' sudah memakai Special Attack!'), nl.

attacked :- 
        chosentokemon(X,_), tokemon(X,HP,A,B,TypeM,E), enemy(C,_,Att,_,TypeL),
        strong(TypeM, TypeL), D is div(Att, 2), Z is HP - D,
        write('Serangannya tidak efektif!'), nl, 
        write(C), write(' menyebabkan '), write(D), write(' damage pada '), write(X), nl, nl,
        retract(tokemon(X,_,_,_,_,_)), asserta(tokemon(X,Z,A,B,TypeM,E)), cekhealthP, !.            
attacked :- 
        chosentokemon(X,_), tokemon(X,HP,A,B,TypeM,E), enemy(C,_,Att,_,TypeL),
        strong(TypeL, TypeM), D is div(Att * 3, 2), Z is HP - D,
        write('Serangannya sangat efektif!'), nl,
        write(C), write(' menyebabkan '), write(D), write(' damage pada '), write(X), nl, nl,   
        retract(tokemon(X,_,_,_,_,_)), asserta(tokemon(X,Z,A,B,TypeM,E)),cekhealthP, !.
attacked :- 
        chosentokemon(X,_), tokemon(X,HP,A,B,TypeM,E), enemy(C,_,Att,_,_),
        Z is (HP - Att),
        write(C), write(' menyebabkan '), write(Att), write(' damage pada '), write(X), nl, nl,   
        retract(tokemon(X,_,_,_,_,_)), asserta(tokemon(X,Z,A,B,TypeM,E)),cekhealthP, !.

life :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,TypeP,_), enemy(Y,HPL,_,_,TypeL),
        write(X), nl, 
        write('Health: '), write(HPP), nl,
        write('Type  : '), write(TypeP), nl, 
		write('========================='),nl,
        write(Y), nl, 
        write('Health: '), write(HPL), nl,
        write('Type  : '), write(TypeL), nl, !.

cekhealthP :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,_,_), HPP =< 0, 
        write(X), write(' meninggal!'),nl,nl,
        retract(inbattle(1)),asserta(inbattle(0)), retract(chosentokemon(X,_)), 
        retract(tokemon(X,Ta,Tb,Tc,Td,Te)),
        asserta(tokemon(X,0,Tb,Tc,Td,Te)),
        retract(banyak(Ta)),
        Tb is Ta+1,
        asserta(banyak(Tb)),
        cektokemon,!.
cekhealthP :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,_,_), HPP > 0, 
        life, !.        

cekhealthL :- 
        enemy(Y,HPL,_,_,_), HPL =< 0, 
        legend_count(Ta),
        Ta \=2,
        write(Y), write(' pingsan! Apakah kamu mau menangkapnya?'),nl,nl,
        write('Jika ingin menangkapnya, berikan perintah capture.'),nl,
        write('Jika tidak ingin, berikan perintah lanjut.'), nl,
        retract(inbattle(1)),asserta(inbattle(2)),!.  
cekhealthL :- 
        enemy(Y,HPL,_,_,_), HPL =< 0, 
        legend_count(Ta),
        Ta ==2,write('Anda Menang HAHAHAHAA!'),halt,nl,!.      
cekhealthL :- 
        enemy(X,HPL,_,_,_), HPL > 0, 
        life,
        write(X), write(' menyerang!'), nl, 
        attacked, !.        
capture:- playerStatus(TokeList,NbToke), 
         NbToke == 6,
         write('TOkemon anda penuh silahkan drop terlebih dahulu!'),nl,!.
capture :-
        \+ loseCondition,
        inbattle(Ta),Ta == 2,
        enemy(X,_,_,_,_), tokemon(A,B,C,D,E,F), id(X,Id),player_location(Xa,Ya),retract(position(Z,Xa,Ya)),playerStatus(TokeList,NbToke),
        append([Id],TokeList,NewTokeList),retract(playerStatus(TokeList,NbToke)),asserta(playerStatus(NewTokeList,NbToke+1)), retract(enemy(X,_,_,_,_)), 
        retract(inbattle(2)),
        nl, map, !.

lanjut :- 
        \+ loseCondition,
        inbattle(2),
        enemy(X,_,_,_,_), 
        write(X), write(' meninggalkan kamu'), nl,
        retract(enemy(X,_,_,_,_,_)), 
        retract(inbattle(2)), 
        nl, map, !.

/* CHECKING TOKEMON IN INVENTORY */

cektokemon :- playerStatus(TokeList,NbToke),
                banyak(Ta),
                Tb is Ta-1,
                Tb \= NbToke,
                cektokemon1(TokeList,NbToke),!.
cektokemon :- playerStatus(TokeList,NbToke),
                banyak(Ta),
                Tb is Ta-1,
                Tb == NbToke,
                write('Maaf anda tidak dapat bertarung lagi, dan anda akan dikeluarkan dari dunia ini hahahaha'),halt,!.
cektokemon1(TokeList,NbToke) :- write('Kamu masih memiliki sisa tokemon'),nl,write('Cek status , dan panggil choose untuk tokemon berikutnya').

/* SWAPPING TOKEMON IN INVENTORY */
change(_) :- 
        loseCondition, lose, !.
change(_) :- 
        \+ loseCondition,
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung!'),nl,!.
/*change(A) :- 
        \+ loseCondition, 
        inbattle(1), 
        \+(tokemon(A,_,_,_,_,_)),
        write('Kamu tidak memiliki Tokemon tersebut!'), nl, !. */
change(A) :- 
        \+ loseCondition, 
        inbattle(T), T == 1, 
        tokemon(A,_,_,_,_,_),
        chosentokemon(X,_), 
        A == X, 
        write('Kamu sedang memakai Tokemon '), write(A), nl, !.
change(A) :- 
        \+ loseCondition, 
        inbattle(T), T == 1, 
        tokemon(A,_,_,_,_,_),
        chosentokemon(X,_), 
        A \= X,
        write('Kembalilah '), write(X), nl,
        retract(chosentokemon(X,_)), asserta(chosentokemon(A,1)),
        write('Keluarlah, '), write(A), nl, !.

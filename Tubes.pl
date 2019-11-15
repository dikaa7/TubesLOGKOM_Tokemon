

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
				X == Xa, X == Ya,
				write('P'),
				M is X+1,
				N is Y,
				printmap(M,N).
printmap(X,Y):- gym_pos1(Xa,Ya),
				X==Xa, Y==Ya,
				write('G'),
				M is X+1,
				N is Y,
				printmap(M,N).
printmap(X,Y):- gym_pos2(Xa,Ya),
				X==Xa,Y==Ya,
				write('G'),
				M is X+1,
				N is Y,
				printmap(M,N).
printmap(X,Y):- X == 22, Y<22,
				write(""),nl,
				M is 0,
				N is Y+1,
				printmap(M,N).
printmap(X,Y):- X==0, Y<22,
				write('X'),
				M is X+1,
				N is Y,
				printmap(M,N).
printmap(X,Y):- Y==0,X<22,
				write('X'),
				M is X+1,
				N is Y,
				printmap(M,N).
printmap(X,Y):- Y==21,X<22,
				write('X'),
				M is X+1,
				N is Y,
				printmap(M,N).
printmap(X,Y):- X==21,X<22,
				write('X'),
				M is X+1,
				N is Y,
				printmap(M,N).
printmap(X,Y):- X<22,Y<22,
				write('-'),
				M is X+1,
				N is Y,
				printmap(M,N).

						

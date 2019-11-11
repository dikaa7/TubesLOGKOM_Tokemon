

help:-		writeln('Available commands:'),
			writeln('	start. --start the game!'),
			writeln('	help. --show available commands'),
			writeln('	quit. --quit the game'),
			writeln('	n. s. e. w. --move'),
			writeln('	map. --look at the map'),
			writeln('	heal --cure Tokemon in inventory if in gym center'),
			writeln('	status. --show your status'),
			writeln('	save(Filename). --save your game'),
			writeln('	load(Filename). --load previously saved game'),
			writeln('Legends : '),
			writeln('- X = Pagar'),
			writeln('- P = Player'),
			writeln('- G = Gym').
			
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
retractall(legend_count(_X)),

init-game:- asserta(player_location(1,1)),
			asserta(gym_pos1(5,4)),
			asserta(gym_pos1(14,12)),
			random(1,12,X),
			id(Y,X),
			asserta(list_pokemon(Y,health(Y),normalAttack(Y),specialAttack(Y))).
			
start:- 	writeln(' ██████╗  ██████╗ ████████╗████████╗ █████╗     '),                                               
		writeln(' ██╔════╝ ██╔═══██╗╚══██╔══╝╚══██╔══╝██╔══██╗   '),                                                
		writeln(' ██║  ███╗██║   ██║   ██║      ██║   ███████║    '),                                               
		writeln(' ██║   ██║██║   ██║   ██║      ██║   ██╔══██║       '),                                            
		writeln(' ╚██████╔╝╚██████╔╝   ██║      ██║   ██║  ██║          '),                                         
		writeln('  ╚═════╝  ╚═════╝    ╚═╝      ╚═╝   ╚═╝  ╚═╝             '),                                      
																										   
		writeln(' ██████╗ █████╗ ████████╗ ██████╗██╗  ██╗    ███████╗███╗   ███╗     █████╗ ██╗     ██╗     ██╗'),
		writeln('██╔════╝██╔══██╗╚══██╔══╝██╔════╝██║  ██║    ██╔════╝████╗ ████║    ██╔══██╗██║     ██║     ██║'),
		writeln('██║     ███████║   ██║   ██║     ███████║    █████╗  ██╔████╔██║    ███████║██║     ██║     ██║'),
		writeln('██║     ██╔══██║   ██║   ██║     ██╔══██║    ██╔══╝  ██║╚██╔╝██║    ██╔══██║██║     ██║     ╚═╝'),
		writeln('╚██████╗██║  ██║   ██║   ╚██████╗██║  ██║    ███████╗██║ ╚═╝ ██║    ██║  ██║███████╗███████╗██╗'),
		writeln(' ╚═════╝╚═╝  ╚═╝   ╚═╝    ╚═════╝╚═╝  ╚═╝    ╚══════╝╚═╝     ╚═╝    ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝'),
		writeln("Gotta catch 'em all!"),

		writeln('Hi there! Welcome to the world of Tokemon! My name is Tome! 								'),
		writeln('There are a lot of tokemon all around you! Catch them to get stronger! '),
		writeln('But your final mission is to defeat or capture all those legendary tokemon, if you fail, '),
		writeln('you will be out from this world.			'),
		help.
maps:- printmap(0,0),!.
printmap(X,Y):- player_pos(Xa,Ya),
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
				writeln(),
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

						

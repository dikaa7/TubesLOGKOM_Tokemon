/* File untuk saat tokemon bertarung */
:- dynamic(lawan/5).
:- dynamic(chosentokemon/2).
:- dynamic(runorfight/0).
:- dynamic(losing/0).

/* Strong Type Tokemon */
strong(fire,grass).
strong(grass,water).
strong(water,fire).
strong(rock,fire).
strong(water,rock).

% Pemilihan tokemon 
choose(_) :- losing, lose, !.
choose(X) :- 
        inbattle(1),
        tokemon(X,_,_,_,_,_,_), asserta(chosentokemon(X,1)),
        %battle stage ke 1 yaitu saat bertarung(attack dan attacked) 
        write('You : Saya memilih kamu,"'),write(X),write('"'),nl,nl, life, !.

/* Bingung ini mau digimanain */
% choose(X) :- 
%         \+ losing,
%         inbattle(1),
%         \+tokemon(X,_,_,_,_), 
%         write('Kamu tidak memiliki pokemon tersebut!, Harap memilih ulang!'), nl, !.
choose(_) :- 
        inbattle(1), 
        chosentokemon(X,_),
        write('Kamu tidak bisa memilih ulang saat bertarung, harap gunakan "change(X)."'),!.
choose(_) :- 
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung'),nl,!.

attack :- 
        losing, lose, !.
attack :-
        inbattle(2),
        write('Tokemonnya sudah pingsan!'), nl,!.
attack :- 
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung'),nl,!.
attack :- 
        inbattle(1), 
        chosentokemon(X,_), tokemon(X,_,Att,_,TypeM,_,_), enemy(A,HP,B,C,TypeL,E),
        strong(TypeM, TypeL), D is div(Att * 3, 2), Z is HP - D,
        write('Serangannya sangat efektif!'), nl,
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)), cekhealthL, !.
attack :- 
        inbattle(1),
        chosentokemon(X,_), tokemon(X,_,Att,_,TypeM,_,_), enemy(A,HP,B,C,TypeL,E),
        strong(TypeL, TypeM), D is div(Att, 2), Z is HP - D, 
        write('Serangannya tidak efektif!'), nl, 
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)), cekhealthL, !.   
attack :- 
        inbattle(1),
        chosentokemon(X,_), tokemon(X,_,Att,_,_,_,_), enemy(A,HP,B,C,TypeL,E),
        Z is (HP - Att),
        write('Kamu menyebabkan '), write(Att), write(' damage pada '), write(A),nl,nl,
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)), cekhealthL, !.

specialAttack :- 
        losing, lose, !.
specialAttack :-
        inbattle(2),
        write('Tokemonnya sudah pingsan!'), nl,!.
specialAttack :-
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung'),nl,!.
specialAttack :- 
        inbattle(1), 
        chosentokemon(X,1), tokemon(X,_,_,Skill,TypeM,_,_), enemy(A,HP,B,C,TypeL,E),
        strong(TypeM, TypeL), D is div(Skill * 3, 2), Z is HP - D,
        write('Serangannya sangat efektif!'), nl,
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)),
        retract(chosentokemonmon(X,1)), asserta(chosentokemonmon(X,0)), cekhealthL, !.              
specialAttack :- 
        inbattle(1), 
        chosentokemon(X,1), tokemon(X,_,_,Skill,TypeM,_,_), enemy(A,HP,B,C,TypeL,E),
        strong(TypeL, TypeM), D is div(Skill, 2), Z is HP - D,
        write('Serangannya tidak efektif!'), nl, 
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)),
        retract(chosentokemonmon(X,1)), asserta(chosentokemonmon(X,0)), cekhealthL, !.
specialAttack :-  
        inbattle(1),
        chosentokemon(X,1), tokemon(X,_,_,Skill,_,_,_), enemy(A,HP,B,C,TypeL,E),
        Z is (HP - Skill),
        write('Kamu menyebabkan '), write(Skill), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)),
        retract(chosentokemonmon(X,1)), asserta(chosentokemonmon(X,0)), cekhealthL, !.
specialAttack :-  
        chosentokemon(X,N), N =< 1, write(X), write(' sudah memakai Skill Attack!'), nl.

attacked :- 
        chosentokemon(X,_), tokemon(X,HP,A,B,TypeM,E,F), enemy(C,_,Att,_,TypeL,_),
        strong(TypeM, TypeL), D is div(Att, 2), Z is HP - D,
        write('Serangannya tidak efektif!'), nl, 
        write(C), write(' menyebabkan '), write(D), write(' damage pada '), write(X), nl, nl,
        retract(tokemon(X,_,_,_,_,_,_)), asserta(tokemon(X,Z,A,B,TypeM,E,F)), cekhealthP, !.            
attacked :- 
        chosentokemon(X,_), tokemon(X,HP,A,B,TypeM,E,F), enemy(C,_,Att,_,TypeL,_),
        strong(TypeL, TypeM), D is div(Att * 3, 2), Z is HP - D,
        write('Serangannya sangat efektif!'), nl,
        write(C), write(' menyebabkan '), write(D), write(' damage pada '), write(X), nl, nl,   
        retract(tokemon(X,_,_,_,_,_,_)), asserta(tokemon(X,Z,A,B,TypeM,E,F)),cekhealthP, !.
attacked :- 
        chosentokemon(X,_), tokemon(X,HP,A,B,TypeM,E,F), enemy(C,_,Att,_,_,_),
        Z is (HP - Att),
        write(C), write(' menyebabkan '), write(Att), write(' damage pada '), write(X), nl, nl,   
        retract(tokemon(X,_,_,_,_,_,_)), asserta(tokemon(X,Z,A,B,TypeM,E,F)),cekhealthP, !.

life :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,TypeP,LevelP,_), enemy(Y,HPL,_,_,TypeL,LevelL),
        write(X), nl, 
        write('Health: '), write(HPP), nl,
        write('Type  : '), write(TypeP), nl, 
        write('Level : '), write(LevelP), nl, nl,
        write(Y), nl, 
        write('Health: '), write(HPL), nl,
        write('Type  : '), write(TypeL), nl, 
        write('Level : '), write(LevelL), nl, nl, !.

cekhealthP :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,_,_,_), HPP =< 0, 
        write(X), write(' meninggal!'),nl,nl,
        retract(inbattle(1)),asserta(inbattle(0)), retract(chosentokemon(X,_)), 
        retract(tokemon(X,_,_,_,_,_,_)),
        cektokemon,!.
cekhealthP :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,_,_,_), HPP > 0, 
        life, !.        

cekhealthL :- 
        enemy(Y,HPL,_,_,_,_), HPL =< 0, 
        write(Y), write(' pingsan! Apakah kamu mau menangkapnya?'),nl,nl,
        write('Jika ingin menangkapnya, berikan perintah capture.'),nl,
        write('Jika tidak ingin, berikan perintah nope.'), nl,
        retract(inbattle(1)),asserta(inbattle(2)),!.        
cekhealthL :- 
        enemy(X,HPL,_,_,_,_), HPL > 0, 
        life,
        write(X), write(' menyerang!'), nl, 
        attacked, !.        

capture :-
        \+ losing,
        inbattle(2),
        enemy(X,_,_,_,_,_), tokemon(X,B,C,D,E,F,G), asserta(avChoose), 
        addtokemon(X,B,C,D,E,F,G), retract(enemy(X,_,_,_,_,_)), 
        retract(inbattle(2)), 
        nl, map, !.

nope :- 
        \+ losing,
        inbattle(2),
        enemy(X,_,_,_,_,_), 
        write(X), write(' pun sadar'), nl,
        write(X), write('(dalam bahasa Tokemon) : Dasar belagu'), nl,
        write(X), write(' meninggalkan kamu'), nl,
        retract(enemy(X,_,_,_,_,_)), 
        retract(inbattle(2)), 
        nl, map, !.

cektokemon :- 
        cektokemon(Banyak), Banyak > 1, !,
        write('Kamu masih memiliki sisa Tokemon!'), nl,
        write('Sisa Tokemon : ['),
        tokemon(H,I,J,K,L,M,N), write(H),
        retract(tokemon(H,I,J,K,L,M)),
        tokemon(_,_,_,_,_,_,_) -> (
                forall(tokemon(A,_,_,_,_,_,_),
                (
                write(','),
                write(A)
                ))
        ),
        write(']'),nl, 
        asserta(tokemon(H,I,J,K,L,M,N)),
        write('Pilih Tokemon sekarang dengan berikan perintah choose(NamaTokemon)'), asserta(inbattle(1)), !.
cektokemon :- 
        cektokemon(Banyak), Banyak =:= 1, 
        write('Sisa Tokemon : ['),
        tokemon(H,_,_,_,_,_,_), write(H),
        write(']'),nl,
        asserta(inbattle(1)), !. 
cektokemon :- 
        cektokemon(Banyak), Banyak =:= 0, asserta(losing), lose,!.

change(_) :- 
        losing, lose, !.
change(_) :- 
        \+ losing,
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung!'),nl,!.
change(A) :- 
        \+ losing, 
        inbattle(1), 
        \+(tokemon(A,_,_,_,_,_,_)),
        write('Kamu tidak memiliki Tokemon tersebut!'), nl, !.
change(A) :- 
        \+ losing, 
        inbattle(1), 
        tokemon(A,_,_,_,_,_,_),
        chosentokemon(X,_), 
        A =:= X, 
        write('Kamu sedang memakai Tokemon '), write(A), nl, !.
change(A) :- 
        \+ losing, 
        inbattle(1), 
        tokemon(A,_,_,_,_,_,_),
        chosentokemon(X,_), 
        A \= X,
        write('Kembalilah '), write(A), nl,
        retract(chosentokemon(X,_)), asserta(chosentokemon(A,1)),
        write('Maju, '), write(A), nl, !.

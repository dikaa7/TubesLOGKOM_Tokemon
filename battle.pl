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
        tokemon(X,_,_,_,_,_), asserta(chosentokemon(X,1)),
        %battle stage ke 1 yaitu saat bertarung(attack dan attacked) 
        write('Keluarlah,"'),write(X),write('"'),nl,nl, life, !.
        
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
        losing, lose, !.
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
        retract(chosentokemonmon(X,1)), asserta(chosentokemonmon(X,0)), cekhealthL, !.              
specialAttack :- 
        inbattle(1), 
        chosentokemon(X,1), tokemon(X,_,_,Special,TypeM,_), enemy(A,HP,B,C,TypeL),
        strong(TypeL, TypeM), D is div(Special, 2), Z is HP - D,
        write('Serangannya tidak efektif!'), nl, 
        write('Kamu menyebabkan '), write(D), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL,E)),
        retract(chosentokemonmon(X,1)), asserta(chosentokemonmon(X,0)), cekhealthL, !.
specialAttack :-  
        inbattle(1),
        chosentokemon(X,1), tokemon(X,_,_,Special,_,_), enemy(A,HP,B,C,TypeL),
        Z is (HP - Special),
        write('Kamu menyebabkan '), write(Special), write(' damage pada '), write(A),nl,nl,   
        retract(enemy(_,_,_,_,_,_)), asserta(enemy(A,Z,B,C,TypeL)),
        retract(chosentokemonmon(X,1)), asserta(chosentokemonmon(X,0)), cekhealthL, !.
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
        chosentokemon(X,_), tokemon(X,HP,A,B,TypeM,E), enemy(C,_,Att,_,_,_),
        Z is (HP - Att),
        write(C), write(' menyebabkan '), write(Att), write(' damage pada '), write(X), nl, nl,   
        retract(tokemon(X,_,_,_,_,_)), asserta(tokemon(X,Z,A,B,TypeM,E)),cekhealthP, !.

life :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,TypeP,_), enemy(Y,HPL,_,_,TypeL),
        write(X), nl, 
        write('Health: '), write(HPP), nl,
        write('Type  : '), write(TypeP), nl, 
      
        write(Y), nl, 
        write('Health: '), write(HPL), nl,
        write('Type  : '), write(TypeL), nl, !.

cekhealthP :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,_,_), HPP =< 0, 
        write(X), write(' meninggal!'),nl,nl,
        retract(inbattle(1)),asserta(inbattle(0)), retract(chosentokemon(X,_)), 
        retract(tokemon(X,_,_,_,_,_)),
        cektokemon,!.
cekhealthP :- 
        chosentokemon(X,_), tokemon(X,HPP,_,_,_,_), HPP > 0, 
        life, !.        

cekhealthL :- 
        enemy(Y,HPL,_,_,_), HPL =< 0, 
        write(Y), write(' pingsan! Apakah kamu mau menangkapnya?'),nl,nl,
        write('Jika ingin menangkapnya, berikan perintah capture.'),nl,
        write('Jika tidak ingin, berikan perintah lanjut.'), nl,
        retract(inbattle(1)),asserta(inbattle(2)),!.        
cekhealthL :- 
        enemy(X,HPL,_,_,_,_), HPL > 0, 
        life,
        write(X), write(' menyerang!'), nl, 
        attacked, !.        

capture :-
        \+ losing,
        inbattle(2),
        enemy(X,_,_,_,_), tokemon(X,B,C,D,E,F), asserta(avChoose), 
        addtokemon(X,B,C,D,E,F), retract(enemy(X,_,_,_,_)), 
        retract(inbattle(2)), 
        nl, map, !.

lanjut :- 
        \+ losing,
        inbattle(2),
        enemy(X,_,_,_,_), 
        write(X), write(' meninggalkan kamu'), nl,
        retract(enemy(X,_,_,_,_,_)), 
        retract(inbattle(2)), 
        nl, map, !.

/* Mengecek Tokemon dalam inventory */
cektokemon :- 
        cektokemon(Banyak), Banyak > 1, !,
        write('Kamu masih memiliki sisa Tokemon!'), nl,
        write('Sisa Tokemon : ['),
        tokemon(H,I,J,K,L,M), write(H),
        retract(tokemon(H,I,J,K,L,M)),
        tokemon(_,_,_,_,_,_) -> (
                forall(tokemon(A,_,_,_,_,_),
                (
                write(','),
                write(A)
                ))
        ),
        write(']'),nl, 
        asserta(tokemon(H,I,J,K,L,M)),
        write('Pilih Tokemon sekarang dengan berikan perintah choose(NamaTokemon)'), asserta(inbattle(1)), !.
cektokemon :- 
        cektokemon(Banyak), Banyak =:= 1, 
        write('Sisa Tokemon : ['),
        tokemon(H,_,_,_,_,_), write(H),
        write(']'),nl,
        asserta(inbattle(1)), !. 
cektokemon :- 
        cektokemon(Banyak), Banyak =:= 0, asserta(losing), lose,!.

/* Tukar tokemon dalam list */
change(_) :- 
        losing, lose, !.
change(_) :- 
        \+ losing,
        \+ inbattle(1), 
        write('Kamu tidak sedang bertarung!'),nl,!.
change(A) :- 
        \+ losing, 
        inbattle(1), 
        \+(tokemon(A,_,_,_,_,_)),
        write('Kamu tidak memiliki Tokemon tersebut!'), nl, !.
change(A) :- 
        \+ losing, 
        inbattle(1), 
        tokemon(A,_,_,_,_,_),
        chosentokemon(X,_), 
        A =:= X, 
        write('Kamu sedang memakai Tokemon '), write(A), nl, !.
change(A) :- 
        \+ losing, 
        inbattle(1), 
        tokemon(A,_,_,_,_,_),
        chosentokemon(X,_), 
        A \= X,
        write('Kembalilah '), write(A), nl,
        retract(chosentokemon(X,_)), asserta(chosentokemon(A,1)),
        write('Maju, '), write(A), nl, !.

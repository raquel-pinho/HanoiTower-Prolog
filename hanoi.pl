%%%Part1 %%%3disks

move([Disc],'A',_,'C', [X,Y,Z], [X2,Y,Z2], C , P,1) :-
    P is C+1,
    reverse(X, [Head|Body]), % Reverse lista do X, Head -> primeiro elemento de X
    reverse(Body, X2), % X2 passa a ser X original sem o primeiro elemento
    reverse(Z, Z1), % Z1 passa a ser reverse Z
    reverse([Head|Z1], Z2), % Z2 passa a ser Z depois de adicionado o primeiro elemento de X
 
    format("Disco ~w movido de A para C", [Disc]),
    nl,
    format("A : ~w , B: ~w , C: ~w", [X2,Y, Z2]),
    nl.
 
move([Disc],'B',_,'C', [X,Y,Z], [X,Y2,Z2], C, P,1) :-
    P is C+1,
    reverse(Y, [Head|Body]), 
    reverse(Body, Y2), 
    reverse(Z, Z1), 
    reverse([Head|Z1], Z2), 
 
    format("Disco ~w movido de B para C", [Disc]),
    nl,
    format("A : ~w , B: ~w , C: ~w", [X,Y2, Z2]),
    nl.
 
move([Disc],'C',_,'B', [X,Y,Z], [X,Y2,Z2], C, P,1) :-
    P is C+1,
    reverse(Z, [Head|Body]), 
    reverse(Body, Z2), 
    reverse(Y, Y1), 
    reverse([Head|Y1], Y2), 
 
    format("Disco ~w movido de C para B", [Disc]),
    nl,
    format("A : ~w , B: ~w , C: ~w", [X,Y2, Z2]),
    nl.
 
move([Disc],'A',_,'B', [X,Y,Z], [X2,Y2,Z], C , P,1) :-
    P is C+1,
    reverse(X, [Head|Body]), 
    reverse(Body, X2), 
    reverse(Y, Y1), 
    reverse([Head|Y1], Y2), 
 
    format("Disco ~w movido de A para B", [Disc]),
    nl,
    format("A : ~w , B: ~w , C: ~w", [X2,Y2, Z]),
    nl.
 
move([Disc],'B',_,'A', [X,Y,Z], [X2,Y2,Z], C, P,1) :-
    P is C+1,
    reverse(Y, [Head|Body]), 
    reverse(Body, Y2), 
    reverse(X, X1), 
    reverse([Head|X1], X2), 
 
    format("Disco ~w movido de B para A", [Disc]),
    nl,
    format("A : ~w , B: ~w , C: ~w", [X2,Y2, Z]),
    nl.
 
move([Disc],'C',_,'A', [X,Y,Z], [X2,Y,Z2], C, P,1) :-
    P is C+1,
    reverse(Z, [Head|Body]), 
    reverse(Body, Z2), 
    reverse(X, X1), 
    reverse([Head|X1], X2), 
    format("Disco ~w movido de C para A", [Disc]),
    nl,
    format("A : ~w , B: ~w , C: ~w", [X2,Y, Z2]),
    nl.
    
 
    
move([Bottom|Rest],Start,Swap,Goal, LL, LL3, C, C3, 0) :- 
 
    move(Rest,Start,Goal,Swap, LL, LL1, C, C1,1), 
    move([Bottom],Start,_,Goal, LL1, LL2, C1, C2,1),
    move(Rest,Swap,Start,Goal, LL2, LL3, C2, C3,1),
    format("Numero de movimentos: ~w", C3).
 
move([Bottom|Rest],Start,Swap,Goal, LL, LL3, C, C3, 1) :- 
    move(Rest,Start,Goal,Swap, LL, LL1, C, C1,1), 
    move([Bottom],Start,_,Goal, LL1, LL2, C1, C2,1),
    move(Rest,Swap,Start,Goal, LL2, LL3, C2, C3,1).
    
 
hanoi(Listas) :-
    move(Listas,'A','B','C', [Listas,[],[]], _, 0, _,0).
 
 
start():-
    write_ln("3 ou 4 torres?"),
    read(Codes),
    
    (Codes == 3 ->
        write_ln("Escreva a lista de discos por ordem decrescente. Ex: [4, 3, 2, 1]"),
        read(Listas),
         hanoi(Listas); 
         write_ln("Escreva o numero de discos"),
         read(N),
         startT(N)).
    
 
 
%%%Part2 %%%4disks
 
startT(N):- hanoi4(N,'A','D','B','C',LL), flatten(LL,LL1), liststolistsoflists(LL1,LL2),getmoves(N,LL2).
 
hanoi4(0,_,_,_,_,[]):-!.
hanoi4(1,FROM,TO,_,_,[1,FROM,TO]):-
    format("[~w,~w,~w]",[1,FROM,TO]),!.
 
hanoi4(N,FROM,TO,AUX1,AUX2,[LL,N2,FROM,AUX2,N,FROM,TO,N2,AUX2,TO,LL1]):-
    N2 is N-1,
    N1 is N-2,
    hanoi4(N1,FROM,AUX1,AUX2,TO,LL),
    hanoi4(N1,AUX1,TO,FROM,AUX2,LL1),!.
 
reversetown(A,B,C,D):-
    reverse(A,[Head|Body]),
    reverse(B,B1),
    reverse([Head|B1],D),
    reverse(Body,C).
 
intToLists(0,[]).
intToLists(N,[N|Rest]):- N1 is N-1, intToLists(N1,Rest).
 
liststolistsoflists([A,B,C],[[A,B,C]]).
liststolistsoflists([A,B,C|Rest],[[A,B,C]|Rest1]):- liststolistsoflists(Rest,Rest1).
 
getmoves(N,LL):- intToLists(N,N1), nl, format("[~w,[],[],[]]",[N1]),nl,getmovesaux(LL,[N1,[],[],[]]).
 
getmovesaux([],_).
getmovesaux([[_,'A','B']|Rest],[A,B,C,D]):- reversetown(A,B,A1,B1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A1,B1,C,D]),nl,  getmovesaux(Rest,[A1,B1,C,D]).
getmovesaux([[_,'A','C']|Rest],[A,B,C,D]):- reversetown(A,C,A1,C1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A1,B,C1,D]),nl,  getmovesaux(Rest,[A1,B,C1,D]).
getmovesaux([[_,'A','D']|Rest],[A,B,C,D]):- reversetown(A,D,A1,D1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A1,B,C,D1]),nl,  getmovesaux(Rest,[A1,B,C,D1]).
getmovesaux([[_,'B','A']|Rest],[A,B,C,D]):- reversetown(B,A,B1,A1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A1,B1,C,D]),nl,  getmovesaux(Rest,[A1,B1,C,D]).
getmovesaux([[_,'B','C']|Rest],[A,B,C,D]):- reversetown(B,C,B1,C1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A,B1,C1,D]),nl,  getmovesaux(Rest,[A,B1,C1,D]).
getmovesaux([[_,'B','D']|Rest],[A,B,C,D]):- reversetown(B,D,B1,D1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A,B1,C,D1]),nl,  getmovesaux(Rest,[A,B1,C,D1]).
getmovesaux([[_,'C','A']|Rest],[A,B,C,D]):- reversetown(C,A,C1,A1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A1,B,C1,D]),nl,  getmovesaux(Rest,[A1,B,C1,D]).
getmovesaux([[_,'C','B']|Rest],[A,B,C,D]):- reversetown(C,B,C1,B1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A,B1,C1,D]),nl,  getmovesaux(Rest,[A,B1,C1,D]).
getmovesaux([[_,'C','D']|Rest],[A,B,C,D]):- reversetown(C,D,C1,D1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A,B,C1,D1]),nl,  getmovesaux(Rest,[A,B,C1,D1]).
getmovesaux([[_,'D','A']|Rest],[A,B,C,D]):- reversetown(D,A,D1,A1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A1,B,C,D1]),nl,  getmovesaux(Rest,[A1,B,C,D1]).
getmovesaux([[_,'D','B']|Rest],[A,B,C,D]):- reversetown(D,B,D1,B1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A,B1,C,D1]),nl,  getmovesaux(Rest,[A,B1,C,D1]).
getmovesaux([[_,'D','C']|Rest],[A,B,C,D]):- reversetown(D,C,D1,C1), format("A: ~w, B: ~w, C: ~w, D: ~w",[A,B,C1,D1]),nl,  getmovesaux(Rest,[A,B,C1,D1]).
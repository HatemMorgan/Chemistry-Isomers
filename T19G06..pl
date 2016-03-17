  straight_chain_alkane(1,A):-A=[carb(h,h,h,h)].
straight_chain_alkane(N,A):-N>1,N1 is N-1,straight_chain_alkane(N1,[H|T]),H = carb(_,Top,Down,Right),H1 = carb(c,Top,Down,Right),A = [carb(h,h,h,c),H1|T].

target_name(N,Target):- N1 is (N*2)+1,atomic_list_concat([c,N,h,N1],Target).

add_branch(carb(L,T,D,R),N,Ans):- T=h,target_name(N,S),Ans=carb(L,S,D,R).

add_branch(carb(L,T,D,R),N,Ans):- T\=h,D=h,target_name(N,S),Ans=carb(L,T,S,R).

break_down(1,[1]).
break_down(N,R):- helper(1,1,N,R).
break_down(N,R):- helper(1,1,N,L),setof(L1,insert(L,L1),L3),split(L3,R).

split([H|_],H).
split([_|T],Ans):-split(T,Ans).

helper(X,N,N,[X]).
helper(X,Y,N,L):-Y<N,X1 is Y+1,helper(X,X1,N,L1),L = [X|L1].

last2([X,Y],X,Y).
last2([_|T],L,L1):-last2(T,L,L1).

add(X,Y,Z):-Z is X+Y.

delete1([X],X,[]).
delete1([H|T],X,L1):- delete1(T,X,L),L1=[H|L].

remove2(L,X,Y,Ans):-delete1(L,X,L1), delete1(L1,Y,L2),Ans=L2  .

insert([X],[X]).
insert([X,Y|T],Ans):- last2([X,Y|T],A,B), remove2([X,Y|T],B,A,L1),  add(A,B,Z),append(L1,[Z],L2), (msort(L2, Ans) ; (insert(L2, Ans), L2 \= Ans) ).
insert([X,Y|T],Ans):- last2([X,Y|T],A,B), remove2([X,Y|T],B,A,L1),  add(A,B,Z),append(L1,[Z],L2),reverse(L2,L3), (msort(L3, Ans) ; (insert(L3, Ans), L3\= Ans) ).

track(P,N,R):-P<N,R is N-P.
track(P,N,R):-P1 is P+1,P1<N,track(P1,N,R).

counter(N,N1):-N1 is N+1.

split2([H|_],H,N,N).
split2([_|T],Ans,N,N2):- counter(N,N1),split2(T,Ans,N1,N2).

replace([_|T], 0, X, [X|T]).
replace([H|T], I, X, [H|R]):- I > -1, NI is I-1, replace(T, NI, X, R).

branched_alkane(N,BA):-track(3,N,R),N1 is N-R,straight_chain_alkane(N1,B),break_down(R,L),hlp2(B,L,BA).

hlp2(B,[],B).
hlp2(B,[H|T],Res):-split2(B,B1,0,C),hlp3(B,B1,H,C,Ans),hlp2(Ans,T,Res).

hlp3(B,B1,A,C,Ans):-B1=carb(c,_,_,c),add_branch(B1,A,B2),replace(B,C,B2,Ans),B2=carb(_,T,D,_),get_number(T,N),get_number(D,N1),length(Ans,L),
Z is C+N+1, Z=<L,K is L-C,Z1 is K+N,Z1=<L,Z2 is C+N1+1,Z2=<L,K1 is L-C,Z3 is K1+N1,Z3=<L.

get_number(h,0).
get_number(C,N):- atom_concat(Y,_,C),atom_concat(X,h,Y),atom_concat(c,N1,X), atom_number(N1,N).

isomers(N,I):-setof(BA,branched_alkane(N,BA),L), mirror2(L,K),mirror3(K,I4), straight_chain_alkane(N, A), Z = [A], append(Z, I4, I).

mirror([],[]).
mirror([H|T],Ans):-H=carb(Lf,T1,D,R), X1=carb(R,D,T1,Lf),  mirror(T, Ans1),  append(Ans1, [X1], Ans).

mirror2([X],[X]).
mirror2([H|T],[H|Ans]):- mirror(H,N),delete(T,N,T1), mirror2(T1,Ans).%Ans=[H|Ans1].

mirror3([], []).
mirror3([X],[X]).
mirror3([H|T],[H|Ans]):- T\= [], H=[A|B],H1=B,last(H1,X),delete(H1,X,H2), reverse(H2,N),append([A],N,N1),append(N1,[X],N2), delete(T,N2,T1),writeln(T1),mirror3(T1,Ans).


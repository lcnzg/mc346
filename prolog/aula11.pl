% tamanho lista
tam([],0).
tam([_|R],N) :- tam(R,NN), N is NN+1.

% tamanho lista com acumulador
tama(L,N) :- tamx(L,N,0).
tamx([],N,Acc) :- Acc=N.
tamx([_|R],N,Acc) :- AA is Acc+1,tamx(R,N,AA).

% soma elementos lista com acumulador
soma(L,N) :- somax(L,N,0).
somax([],N,Acc) :- Acc=N.
somax([X|R],N,Acc) :- AA is Acc+X, somax(R,N,AA).

% verifica se eh par
par(X) :- 0 is mod(X,2).

% soma numeros pares da lista
somap([],0).
somap([X|R],N) :- somap(R,NN), par(X), N is NN+X.
somap([_|R],N) :- somap(R,N).

% soma dos elementos nas posições pares da lista ( o primeiro elemento esta na posicao 1) somapares(+LISTA,-SOMA)
somapares(L,N) :- somaparesx(L,N,1,0).
somaparesx([],N,_,Acc) :- Acc=N.
somaparesx([X|R],N,Pos,Acc) :- par(Pos), AA is Acc+X, P is Pos+1, somaparesx(R,N,P,AA).
somaparesx([_|R],N,Pos,Acc) :- P is Pos+1, somaparesx(R,N,P,Acc).

% existe item na lista elem(+IT,+LISTA)
existe(X,[X|R]).
existe(X,[Y|R]) :- existe(X,R).

% posição do item na lista: 1 se é o primeiro, falha se nao esta na lista
pos(X,L,P) :- posx(X,L,P,1).
posx(X,[X|_],Acc,Acc).
posx(X,[Y|R],P,Acc) :- Acc2 is Acc+1, posx(X,R,P,Acc2).

% conta quantas vezes o item aparece na lista (0 se nenhuma)
conta(X,L,C) :- contax(X,L,C,0).
contax(_,[],Acc,Acc).
contax(X,[X|R],C,Acc) :- Acc2 is Acc+1, contax(X,R,C,Acc2).
contax(X,[Y|R],C,Acc) :- contax(X,R,C,Acc).

% maior elemento de uma lista - maior(+LISTA,-MAX)
maior([X],X).
maior([X|R],M) :- maior(R,Z), X > Z, M is X.
maior([X|R],M) :- maior(R,M).

% reverte uma lista
reverte(L,R) :- revertex(L,R,[]).
revertex([],Acc,Acc).
revertex([X|R],L,Acc) :- revertex(R,L,[X|Acc]).

% intercala 2 listas (intercala1 e intercala2)
intercala1([],_,[]).
intercala1(_,[],[]).
intercala1([X|R1],[Y|R2],I) :- intercala1(R1,R2,I2), I = [X|[Y|I2]].

intercala2([],X,X).
intercala2(X,[],X).
intercala2([X|R1],[Y|R2],I) :- intercala2(R1,R2,I2), I = [X|[Y|I2]].

% a lista ja esta ordenada?
ordenada([]).
ordenada([_]).
ordenada([X|[Y|R]]) :- X < Y, ordenada([Y|R]).

% dado n gera a lista de 1 a n
gera_lista(N,L) :- gera_listax(N,L,1).
gera_listax(N,[N],N).
gera_listax(N,L,C) :- C2 is C+1, gera_listax(N,Z,C2), L = [C|Z].

% retorna o ultimo elemento de uma lista
ultimo([X],X).
ultimo([X|R],Y) :- ultimo(R,Y).

% retorna a lista sem o ultimo elemento
inicio([X],[]).
inicio([X|R],L) :- inicio(R,Z), L = [X|Z].

% shift right
split_last([X],[],X).
split_last([X|R],L,Y) :- split_last(R,Z,Y), L = [X|Z].

shiftr(L,X) :- split_last(L,Z,Y), X = [Y|Z].

% shiftr n lista (shift right n vezes)
shiftr_n(L,0,L).
shiftr_n(L,N,R) :- N2 is N-1, shiftr_n(L,N2,Z), shiftr(Z,R).

% shift left
shiftl([X|R],L) :- shiftlx(R,X,L).
shiftlx([],X,[X]).
shiftlx([X|R],Y,L) :- shiftlx(R,Y,Z), L = [X|Z].

% shift left n vezes
shiftl_n(L,0,L).
shiftl_n(L,N,R) :- N2 is N-1, shiftl_n(L,N2,Z), shiftl(Z,R).

% remove item da lista (1 vez so)
remove(_,[],[]).
remove(X,[X|R],R).
remove(X,[Y|R],W) :- remove(X,R,Z), W = [Y|Z].

% remove item da lista (todas as vezes)
remove_todas(_,[],[]).
remove_todas(X,[X|R],W) :- remove_todas(X,R,W).
remove_todas(X,[Y|R],W) :- remove_todas(X,R,Z), W = [Y|Z].

% remove item da lista n (as primeiras n vezes)
remove_n(_,0,L,L).
remove_n(X,N,L,R) :- N2 is N-1, remove_n(X,N2,L,Z), remove(X,Z,R).

% remove item da lista (a ultima vez que ele aparece) **
remove_ult(X,L,W) :- reverte(L,Z), remove(X,Z,T), reverte(T,W).

% troca velho por novo na lista (1 so vez)
troca(_,_,[],[]).
troca(X,Y,[X|R],[Y|R]).
troca(X,Y,[H|R],W) :- troca(X,Y,R,Z), W = [H|Z].

% troca velho por novo na lista (todas vezes)
troca_todas(_,_,[],[]).
troca_todas(X,Y,[X|R],W) :- troca_todas(X,Y,R,Z), W = [Y|Z].
troca_todas(X,Y,[H|R],W) :- troca_todas(X,Y,R,Z), W = [H|Z].

% troca velho por novo na lista n (as primeiras n vezes)
troca_n(_,_,0,L,L).
troca_n(X,Y,N,L,R) :- N2 is N-1, troca_n(X,Y,N2,L,Z), troca(X,Y,Z,R).

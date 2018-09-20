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

% posição do item na lista: 1 se é o primeiro, falha se nao esta na lista pos(+IT,+LISTA,-POS)

% conta quantas vezes o item aparece na lista (0 se nenhuma) conta(+IT,+LISTA,-CONTA)

% maior elemento de uma lista - maior(+LISTA,-MAX)

% reverte uma lista -

% intercala 2 listas (intercala1 e intercala2)

% a lista ja esta ordenada?

% dado n gera a lista de 1 a n

% retorna o ultimo elemento de uma lista

% retorna a lista sem o utlimo elemento

% shift right

% shiftr n lista (shift right n vezes)

% shift left

% shift left n vezes

% remove item da lista (1 vez so)

% remove item da lista (todas as vezes)

% remove item da lista n (as primeiras n vezes)

% remove item da lista (a ultima vez que ele aparece) **

% troca velho por novo na lista (1 so vez)

% troca velho por novo na lista (todas vezes)

% troca velho por novo na lista n (as primeiras n vezes)

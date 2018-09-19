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

% soma elementos pares da lista
somap([],0).
somap([X|R],N) :- somap(R,NN), par(X), N is NN+X.
somap([_|R],N) :- somap(R,N).

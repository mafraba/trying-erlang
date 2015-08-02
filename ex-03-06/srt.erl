-module(srt).
-compile(export_all).
-author('mafraba@gmail.com').

%%
quicksort([]) -> [];
quicksort([H|T]) -> quicksort(lt(H,T)) ++ [H] ++ quicksort(ge(H,T)).
lt(_,[]) -> [];
lt(X,[H|T]) when H<X -> [H|lt(X,T)];
lt(X,[H|T]) when H>=X -> lt(X,T).
ge(_,[]) -> [];
ge(X,[H|T]) when H>=X -> [H|ge(X,T)];
ge(X,[H|T]) when H<X -> ge(X,T).

%%
mergesort([H|[]]) -> [H];
mergesort(L=[_|_]) -> 
	% split in aprox sizes
	{L1, L2} = lists:split(length(L) div 2, L),
	% sort each one
	SL1 = mergesort(L1),
	SL2 = mergesort(L2),
	% merge them
	lists:merge(SL1,SL2).
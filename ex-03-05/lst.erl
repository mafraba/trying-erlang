-module(lst).
-compile(export_all).
-author('mafraba@gmail.com').

%%
filter([],_) -> [];
filter([H|T], N) when H=<N -> [H|filter(T,N)];
filter([H|T], N) when H>N -> filter(T,N).

%%
reverse(X) -> reverse(X,[]).
reverse([], Acc) -> Acc;
reverse([H|T], Acc) -> reverse(T,[H|Acc]).

%% 
concatenate([]) -> [];
concatenate([H|T]) -> H ++ concatenate(T).

%%
flatten([]) -> [];
flatten([H|T]) -> flatten(H) ++ flatten(T);
flatten(E) -> [E].
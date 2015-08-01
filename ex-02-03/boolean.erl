%%
%% Excercise 2-3 from 'Erlang Programming' book
%% by mafraba@gmail.com
%%

-module(boolean).
-compile(export_all).
-author('mafraba@gmail.com').


%% Negation
b_not(false) -> true;
b_not(_) -> false.

%% 
b_and(true, true) -> true;
b_and(_,_) -> false.

%%
b_or(false, false) -> false;
b_or(_,_) -> true.

%% 
b_nand(A,B) -> b_not(b_and(A,B)).


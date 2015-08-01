-module (sum).
-compile(export_all).
-author('mafraba@gmail.com').

%%
sum(0) -> 0;
sum(N) when N>0 -> N + sum(N-1);
sum(_) -> exit(badarith).

%%
sum(N,M) when N>M -> exit(badarith);
sum(N,N) -> N;
sum(N,M) -> M + sum(N,M-1).
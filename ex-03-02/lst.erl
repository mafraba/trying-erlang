-module(lst).
-compile(export_all).
-author('mafraba@gmail.com').

%%
create(N) when N > 0 -> create(1,N).
create(N,N) -> [N];
create(N,M) -> [N|create(N+1,M)].

%%
reverse_create(0) -> [];
reverse_create(N) -> [N|reverse_create(N-1)].
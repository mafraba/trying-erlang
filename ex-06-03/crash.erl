-module(crash).
-author('mafraba@gmail.com').
-export([start/0]).


%%% PRIVATE %%%

crash() ->
  exit(crash).

%%% EXPORTED %%%

%% Inits the echo server process
start() ->
  Pid = spawn_link(fun crash/0),
  % register(crash, Pid),
  {ok, Pid}.

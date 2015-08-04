%%
%% Excercise 4-1 from 'Erlang Programming' book
%% by mafraba@gmail.com
%%

-module(echo).
-author('mafraba@gmail.com').
-export([start/0, stop/0, print/1]).


%%% PRIVATE %%%

loop() ->
  receive
    {print, Msg} -> io:format("~w~n",[Msg]), loop();
    stop -> ok
  end.

%%% EXPORTED %%%

%% Inits the echo server process
start() ->
  catch register(echo, spawn(fun loop/0)),
  ok.

%% Stops the echo server process
stop() ->
  Pid = whereis(echo),
  Pid ! stop,
  ok.

%% Prints a message
print(Msg) ->
  Pid = whereis(echo),
  Pid ! {print, Msg},
  ok.


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
  Pid = spawn_link(fun loop/0),
  register(echo, Pid),
  {ok, Pid}.

%% Stops the echo server process
stop() ->
  whereis(echo) ! stop.

%% Prints a message
print(Msg) ->
  Pid = whereis(echo),
  Pid ! {print, Msg},
  ok.

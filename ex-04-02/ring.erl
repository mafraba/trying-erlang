%%
%% Excercise 4-2 from 'Erlang Programming' book
%% by mafraba@gmail.com
%%

-module(ring).
-author('mafraba@gmail.com').
-export([start/3]).


%%% EXPORTED %%%

%% Inits the ring: M messages over N processes
start(M, N, Message) ->
  io:format("Start!~n",[]),
  % setup the N processes
  First = setup(N),
  % send M messages
  send(M, Message, First),
  % return ok
  ok.


%%% PRIVATE %%%

%% Basic process work: forward message to next process (or quit)
proc(NextNode) ->
  receive
    quit -> 
      NextNode ! quit,
      io:format("~w quitting~n",[self()]),
      ok;
    {0, _} ->
      io:format("~w ttl expired, firing quit message~n",[self()]),
      NextNode ! quit,
      proc(NextNode);
    {M, Msg} -> 
      io:format("~w received ~w, forwarding to ~w (ttl: ~w)~n",[self(), Msg, NextNode, M]),
      NextNode ! {M-1, Msg}, 
      proc(NextNode)
  end.

%% Setup
setup(TargetCount) ->
  io:format("~w spawing first process~n",[self()]),
  % First process passes itself as such
  spawn(fun() -> proc(setup(TargetCount, 1, self())) end).
setup(TargetCount, TargetCount, First) ->
  io:format("~w is last process~n",[self()]),
  % All created, last process will receive the first one
  First;
setup(TargetCount, AlreadySetup, First) when AlreadySetup<TargetCount ->
  io:format("~w spawing next process (status:~w) ~n",[self(), {TargetCount, AlreadySetup}]),
  % Create next process and save its pid
  spawn(fun() -> proc(setup(TargetCount, AlreadySetup+1, First)) end).


%% Send M-hops message
send(M, Msg, Pid) -> 
  io:format("~w sent to ~w~n",[Msg,Pid]),
  Pid ! {M, Msg}.


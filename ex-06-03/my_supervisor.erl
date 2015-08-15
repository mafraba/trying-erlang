-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

% The supervisor takes a list of tuples of the form {Module, Function, Arguments, Type} .
% This list describes the children that the supervisor has to supervise by giving
% the functions that have to be called to start the child processes. In doing this,
% we assume that the child process is started using the spawn_link/3 BIFs, and that
% the function, if successful, returns the tuple {ok, Pid}.
% The Type parameter can be set to permanent or transient: if the child is transient,
% it is not restarted if it terminated normally, but only upon abnormal termination.

start_link(Name, ChildSpecList) ->
  register(Name, spawn_link(my_supervisor, init, [ChildSpecList])),
  ok.

init(ChildSpecList) ->
  process_flag(trap_exit, true),
  loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{Mod, Func, Args, Type} | ChildSpecList]) ->
  case (catch apply(Mod, Func, Args)) of
    {ok, Pid} ->
      [{Pid, {Mod, Func, Args, Type}}|start_children(ChildSpecList)];
    _ ->
      start_children(ChildSpecList)
  end.

% The loop of the supervisor waits in a receive clause for EXIT and stop messages. If a
% child terminates, the supervisor receives the EXIT signal and restarts the terminated
% child, replacing its entry in the list of children stored in the ChildList variable.
% If the child is transient, it is not restarted if it terminated normally, but
% only upon abnormal termination.

restart_child(Pid, ChildList, Reason) ->
  {Pid, {M,F,A,T}} = lists:keyfind(Pid, 1, ChildList),
  case T of
    transient when Reason == normal ->
      io:format("Normal termination of transient child ~w~n",[Pid]),
      lists:keydelete(Pid,1,ChildList);
    _ ->
      {ok, NewPid} = apply(M,F,A),
      io:format("Restarting child ~w, now it's ~w~n",[Pid, NewPid]),
      [{NewPid, {M,F,A,T}}|lists:keydelete(Pid,1,ChildList)]
  end.

loop(ChildList) ->
  receive
    {'EXIT', Pid, Reason} ->
      io:format("Got child EXIT: ~w / ~w / ~w~n",[Pid, Reason, ChildList]),
      NewChildList = restart_child(Pid, ChildList, Reason),
      loop(NewChildList);
    {stop, From} ->
      From ! {reply, terminate(ChildList)}
  end.

% We stop the supervisor by calling the synchronous client function stop/0 .
% Upon receiving the stop message, the supervisor runs through the ChildList ,
% terminating the children one by one. Having terminated all the children, the
% atom ok is returned to the process that initiated the stop call.

stop(Name) ->
  Name ! {stop, self()},
  receive {reply, Reply} -> Reply end.

terminate([{Pid, _} | ChildList]) ->
  exit(Pid, kill),
  terminate(ChildList);
terminate(_ChildList) -> ok.

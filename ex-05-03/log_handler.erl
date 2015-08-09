-module(log_handler).
-export([init/1, terminate/1, handle_event/2]).

init(File) ->
  {ok, Fd} = file:open(File, write),
  Fd.

terminate(Fd) ->
  {ok, File} = file:pid2name(Fd),
  % io:format("Closing ~w~n",[File]),
  file:close(Fd),
  {ok, File}.

handle_event({Action, Id, Event}, Fd) ->
  {MegaSec, Sec, MicroSec} = now(),
  io:format(Fd, "~w,~w,~w,~w,~w,~p~n",
                [MegaSec, Sec, MicroSec, Action, Id, Event]),
  Fd;
handle_event(_, Fd) ->
  Fd.

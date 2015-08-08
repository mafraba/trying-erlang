-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1, list_freqs/0]).
-export([init/0]).

%% These are the start functions used to create and
%% initialize the server.

init() ->
  % Tuple of available and already allocated freqs
  Frequencies = {get_frequencies(), []},
  loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].


%% The client Functions

start()           -> register(frequency, spawn(frequency, init, [])).
stop()            -> call(stop).
allocate()        -> call(allocate).
deallocate(Freq)  -> call({deallocate, Freq}).
list_freqs()      -> call(list).


%% We hide all message passing and the message
%% protocol in a functional interface.

call(Message) ->
  frequency ! {request, self(), Message},
  receive
    {reply, Reply} -> Reply
  end.


%% The Main Loop

loop(Frequencies) ->
  receive
    {request, Pid, allocate} ->
      {NewFrequencies, Reply} = allocate(Frequencies, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid , {deallocate, Freq}} ->
      {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
      reply(Pid, Reply),
      loop(NewFrequencies);
    {request, Pid, stop} ->
      case Frequencies of
        {_,[]} ->
          reply(Pid, ok);
        _ ->
          reply(Pid, {error, frequencies_allocated}),
          loop(Frequencies)
      end;
    {request, Pid, list} ->
      reply(Pid, {frequencies, Frequencies}),
      loop(Frequencies)
  end.

reply(Pid, Reply) ->  Pid ! {reply, Reply}.


%% The Internal Help Functions used to allocate and
%% deallocate frequencies.

% no free frequencies case
allocate({[], Allocated}, _Pid) ->
  {{[], Allocated}, {error, no_frequency}};
% successful allocation case
allocate({[Freq|Free], Allocated}, Pid) ->
  {{Free, [{Freq, Pid}|Allocated]}, {ok, Freq}}.

%% successful deallocation case
deallocate({Free, Allocated}, Freq, Pid) ->
  case lists:member({Freq,Pid},Allocated) of
    true ->
      NewAllocated = lists:delete({Freq, Pid}, Allocated),
      {{[Freq|Free], NewAllocated}, ok};
    false ->
      {{Free, Allocated}, error}
  end.

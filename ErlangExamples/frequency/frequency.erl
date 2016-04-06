%% Code from 
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(frequency).
-export([start/0, stop/0, allocate/0, deallocate/1]).
-export([init/0, init/3]).

%% These are the start functions used to create and
%% initialize the server.

start() ->
    register(frequency, spawn(frequency, init, [])).

init() -> init(3, noop, lifo).
init(FreqsOpt, DeallocOpt, AllocOpt) ->
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(FreqsOpt), []},
    Deallocate = fun_for_dealloc(DeallocOpt),
    Allocate = fun_for_alloc(AllocOpt),
    loop(Frequencies, {Deallocate, Allocate}).

fun_for_dealloc(cannot) -> fun deallocate/2;
fun_for_dealloc(Op) -> fun (X, Y) -> deallocate_custom(Op, X, Y) end.

fun_for_alloc(lifo) -> fun allocate/2;
fun_for_alloc(smallf) -> fun ({Freqs, Alloc}, Pid) -> allocate({lists:sort(Freqs), Alloc}, Pid) end.

get_frequencies(Num) -> lists:seq(10, Num + 9).

%%  The client Functions

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

%% We hide all message passing and the message
%% protocol in a functional interface.
call(Message) ->
  frequency ! {request, self(), Message},
    receive
	{reply, Reply} -> Reply
    end.

reply(Pid, Message) ->
    Pid ! {reply, Message}.

loop(Frequencies, {Deallocate, Allocate} = Ops) ->
    receive
	{request, Pid, allocate} ->
	    {NewFrequencies, Reply} = Allocate(Frequencies, Pid),
	    reply(Pid, Reply),
	    loop(NewFrequencies, Ops);
	{request, Pid , {deallocate, Freq}} ->
	    NewFrequencies = Deallocate(Frequencies, Freq),
	    reply(Pid, ok),
	    loop(NewFrequencies, Ops);
	{'EXIT', Pid, _Reason} ->
	    NewFrequencies = exited(Frequencies, Pid),
	    loop(NewFrequencies, Ops);
	{request, Pid, stop} ->
	    reply(Pid, ok)
    end.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequencies}};
allocate({[Freq|Frequencies], Allocated}, Pid) ->
    link(Pid),
    {{Frequencies,[{Freq,Pid}|Allocated]},{ok,Freq}}.

deallocate({Free, Allocated}, Freq) ->
    {value,{Freq,Pid}} = lists:keysearch(Freq,1,Allocated),
    unlink(Pid),
    NewAllocated=lists:keydelete(Freq,1,Allocated),
    {[Freq|Free], NewAllocated}.

deallocate_custom(Behaviour, {Free, Allocated}, Freq) ->
    case {Behaviour, lists:keysearch(Freq,1,Allocated)} of
	{_, {value,{Freq,Pid}}} -> unlink(Pid),
				   NewAllocated=lists:keydelete(Freq,1,Allocated),
				   {[Freq|Free],  NewAllocated};
	{extra_copy, _} -> {[Freq|Free], Allocated};
	{noop, _} -> {Free, Allocated}
    end.

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid,2,Allocated) of
	{value,{Freq,Pid}} ->
	    NewAllocated = lists:keydelete(Freq,1,Allocated),
	    {[Freq|Free],NewAllocated};
	false ->
	    {Free,Allocated}
    end.

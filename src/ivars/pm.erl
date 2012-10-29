-module(pm).

-export([newVanilla/0, newPrincess/1, get/1, put/2, compromised/1]).

-record(vState, {value, compromised}).

newVanilla() ->
    Pid = spawn(fun vanilla_unset/0),
    {vanilla, Pid}.

newPrincess(P) ->
    Pid = spawn(fun() -> princess_unset(P) end),
    {princess, Pid}.

get({_Flavour, Pid}) -> rpc(Pid, get).
put({_Flavour, Pid}, T) -> rpc(Pid, {put, T}).

compromised({vanilla, Pid})   -> rpc(Pid, compromised);
compromised({princess, _Pid}) -> false.

rpc(Pid, Request) ->
    Pid ! {self(), Request},
    receive
    {Pid, Response} ->
        Response
    end.

reply(From, Msg) ->
    From ! {self(), Msg}.

vanilla_unset() ->
    receive
        {From, {put, T}} ->
            reply(From, ok),
            vanilla_set(#vState{value=T, compromised=false});
        {From, compromised} ->
            reply(From, false),
            vanilla_unset()
    end.

vanilla_set(S = #vState{value=V, compromised=C}) ->
    receive
        {From, compromised} ->
            reply(From, C),
            vanilla_set(S);
        {From, {put, _T}} ->
            reply(From, {error, already_set}),
            vanilla_set(S#vState{compromised=true});
        {From, get} ->
            reply(From, {ok, V}),
            vanilla_set(S)
    end.

princess_unset(P) ->
    receive
        {From, {put, T}} ->
            reply(From, ok),
            try P(T) of
                true -> princess_set(T);
                _    -> princess_unset(P)
            catch
                _ -> princess_unset(P)
            end
    end.

princess_set(V) ->
    receive
        {From, {put, _}} ->
            reply(From, {error, already_set}),
            princess_set(V);
        {From, get} ->
            reply(From, {ok, V}),
            princess_set(V)
    end.

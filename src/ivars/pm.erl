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
put({_Flavour, Pid}, T) -> Pid ! {put, T}, T.

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
        {put, T} ->
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
        {put, _T} ->
            vanilla_set(S#vState{compromised=true});
        {From, get} ->
            reply(From, V),
            vanilla_set(S)
    end.

princess_unset(P) ->
    receive
        {put, T} ->
            try P(T) of
                true -> princess_set(T);
                _    -> princess_unset(P)
            catch
                _ -> princess_unset(P)
            end
    end.

princess_set(V) ->
    receive
        {put, _} ->
            princess_set(V);
        {From, get} ->
            reply(From, V),
            princess_set(V)
    end.

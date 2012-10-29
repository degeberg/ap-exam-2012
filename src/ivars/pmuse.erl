-module(pmuse).
-export([pmmap/2, treeforall/2]).

-import(pm).

pmmap(F, L) ->
    Me = self(),
    Pids = lists:map(fun(V) -> spawn(fun() -> run_mapper(Me, F, V) end) end, L),
    gather_map_result(Pids, []).

run_mapper(From, F, V) ->
    {ok, X} = pm:get(V),
    Res = F(X),
    From ! {self(), Res}.

gather_map_result([], L) -> L;
gather_map_result([Pid | Pids], L) ->
    receive
        {Pid, Res} ->
            gather_map_result(Pids, L ++ [Res])
    end.


treeforall(T, P) ->
    Nodes = preorder(T),
    Me = self(),
    Pids = lists:map(fun(V) -> spawn(fun() -> run_mapper(Me, P, V) end) end, Nodes),
    gather_forall_result(Pids).

gather_forall_result([]) -> true;
gather_forall_result(Pids) ->
    receive
        {_Pid, false} ->
            false;
        {Pid, true} ->
            NewPids = lists:delete(Pid, Pids),
            gather_forall_result(NewPids)
    end.

preorder(leaf) -> [];
preorder({node, X, L, R}) ->
    [X] ++ preorder(L) ++ preorder(R).

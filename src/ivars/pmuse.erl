-module(pmuse).
-export([pmmap/2, treeforall/2]).

-import(pm).

pmmap(F, L) ->
    IVars = lists:map(fun(T) ->
                V = pm:newVanilla(),
                spawn(fun() -> pm:put(V, F(T)) end),
                V
        end, L),
    lists:map(fun pm:get/1, IVars).

treeforall(T, P) ->
    Parent = self(),
    spawn(fun() ->
        Me = self(),
        P2 = fun(X) -> Me ! try P(X) catch _ -> false end end,
        Parent ! gather_forall_result(traverse(T, P2))
    end),
    receive
        true -> true;
        _    -> false
    end.

gather_forall_result(0) -> true;
gather_forall_result(N) ->
    receive
        false -> false;
        true  -> gather_forall_result(N - 1)
    end.

traverse(leaf, _P) -> 0;
traverse({node, X, L, R}, P) ->
    V = pm:newPrincess(P),
    pm:put(V, X),
    1 + traverse(L, P) + traverse(R, P).

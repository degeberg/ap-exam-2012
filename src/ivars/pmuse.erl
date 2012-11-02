-module(pmuse).
-export([pmmap/2, treeforall/2]).

-import(pm).

-include_lib("eunit/include/eunit.hrl").

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

% Tests:

pmmap_empty_test() ->
    ?assertEqual(pmmap(fun (X) -> X end, []), []).

pmmap_id_test() ->
    ?assertEqual(pmmap(fun (X) -> X end, [1, 2, 3]), [1, 2, 3]).

pmmap_plus1_test() ->
    ?assertEqual(pmmap(fun (X) -> X + 1 end, [1, 2, 3]), [2, 3, 4]).

treeforall_empty_test() ->
    ?assert(treeforall(leaf, fun (_) -> true end)),
    ?assert(treeforall(leaf, fun (_) -> false end)).

treeforall_single_true_test() ->
    ?assert(treeforall({node, 1, leaf, leaf}, fun(_) -> true end)).

treeforall_single_false_test() ->
    ?assertNot(treeforall({node, 1, leaf, leaf}, fun(_) -> false end)).

treeforall_shortcircuit_test() ->
    ?assertNot(treeforall({node, 1, {node, 2, leaf, leaf}, leaf}, fun(X) -> X =:= 1 end)).

treeforall_multiple_true_test() ->
    ?assert(treeforall({node, 1, {node, 2, leaf, leaf}, leaf}, fun(_) -> true end)).

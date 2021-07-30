-module(rosalind_math).

-export([fib/2, fibd/2, iprb/3, iev/6, lia/2, perm/1,
         rosalind_rounding/1, signed_permutations/1,
         rosalind_rounding/2, complete_tree/2, longest_subsequences/1,
         fact/1, partial_permutations/2, tree_nodes/1, subset_count/1,
         count_reversals/2]).

-compile(export_all).

fib(N, K) ->
    fib(N, K, 1, 0).

fib(1, _K, Child, Adult) -> Child + Adult;
fib(N, K, Child, Adult) ->
    fib(N - 1, K, Adult * K, Adult + Child).

fibd(N, M) ->
    fibd(1, N, M, 1, 0, []).

fibd(N, N, _M, Child, Adult, _History)  ->
    Child + Adult;
fibd(I, N, M, Child, Adult, History) when I >= M ->
    fibd(I + 1, N, M, Adult, Child + Adult - lists:nth(M - 1, History),
         [Child|History]);
fibd(I, N, M, Child, Adult, History) ->
    fibd(I + 1, N, M, Adult, Child + Adult, [Child|History]).

iprb(K, M, N) ->
    Total = K + M + N,
    KPercent = K / Total,
    MInitial = M * 0.5 / Total,
    NInitial = N / Total,

    MK = MInitial * K / (Total - 1),
    MM = MInitial * (M - 1) * 0.5 / (Total - 1),
    NK = NInitial * K / (Total - 1),
    NM = NInitial * M * 0.5 / (Total - 1),

    MInitial + KPercent + MK + MM + NK + NM.

iev(A, B, C, D, E, _F) ->
    (A + B + C) * 2 + D * 1.5 + E.

lia(K, N) ->
    Children = round(math:pow(2, K)),
    Fn = fun(X) ->
                 BC = binomial_coefficient(Children, X),
                 Out = math:pow(0.75, Children - X),
                 In = math:pow(0.25, X),
                 BC * In * Out
         end,
    rosalind_rounding(1 - lists:sum(lists:map(Fn, lists:seq(0, N - 1)))).

perm(N) ->
    perm_(lists:seq(1, N)).

signed_permutations(N) ->
    Pos = perm(N),
    lists:flatmap(fun sign_perms_/1, Pos).

% tree
complete_tree(N, List) ->
    Fn = fun({K, V}, Acc) ->
                 For = maps:get(K, Acc, []),
                 Back = maps:get(V, Acc, []),
                 Acc#{K => [V|For], V => [K|Back]}
         end,
    Map = lists:foldl(Fn, #{}, List),
    count_trees(lists:seq(1, N), [], Map, 0).

% lia
rosalind_rounding(N) ->
    rosalind_rounding(N, 3).

% lia
rosalind_rounding(N, E) ->
    Pow = math:pow(10, E),
    round(N * Pow) / Pow.

% lia
fact(N) -> fact(N, 1).

fact(0, Acc) -> Acc;
fact(N, Acc) ->
    fact(N - 1, Acc * N).

% lgis
longest_subsequences(List) ->
    Inc = fun(A, B) -> A < B end,
    Dec = fun(A, B) -> A > B end,
    {find_sequence(Inc, List), find_sequence(Dec, List)}.

% pper
partial_permutations(N, K) ->
    floor(fact(N) / fact(N - K)).

% inod
tree_nodes(N) when N < 3 -> 0;
tree_nodes(N) -> N - 2.

% sset
subset_count(N) ->
    floor(math:pow(2, N)).

% rear
count_reversals(L, O) ->
    reversal_distance(sets:from_list([L]), sets:from_list([O]), 0, #{}).

%% helper methods

% lia
binomial_coefficient(N, K) ->
    fact(N) / (fact(K) * fact(N - K)).

% perm
perm_([]) -> [[]];
perm_(L) -> [[H|T] || H <- L, T <- perm_(L--[H])].

% sign
sign_perms_([X]) -> [[X], [-X]];
sign_perms_([H|T]) ->
    Base = sign_perms_(T),
    Fn = fun(B) -> [[H|B], [-H|B]] end,
    lists:flatmap(Fn, Base).

% tree
count_trees([], _V, _Map, N) -> N - 1;
count_trees([H|T], Visited, Map, N) ->
    {NewNodes, NewVisited} = consume_nodes([H], T, Visited, Map),
    count_trees(NewNodes, NewVisited, Map, N + 1).

% tree
consume_nodes([], Nodes, Visited, _Map) -> {Nodes, Visited};
consume_nodes([H|T], Nodes, Visited, Map) ->
    New = lists:delete(H, Nodes),
    Children = maps:get(H, Map, []) -- Visited,
    consume_nodes(Children ++ T, New, [H|Visited], Map).

% lgis
find_sequence(Cmp, List) ->
    N = length(List),
    P = array:new(N),
    M = array:new(N + 1),
    X = array:from_list(List),
    {L0, P0, M0} = lgis(Cmp, 0, 0, N, X, P, M),
    rebuild_sequence(L0 - 1, array:get(L0, M0), X, P0, []).

% lgis
lgis(_Cmp, N, L, N, _X, P, M) -> {L, P, M};
lgis(Cmp, I, L, N, X, P, M) ->
    NewL = lgis_bin_search(Cmp, I, 1, L, X, M),
    P0 = array:set(I, array:get(NewL - 1, M), P),
    M0 = array:set(NewL, I, M),
    if NewL > L ->
           lgis(Cmp, I + 1, NewL, N, X, P0, M0);
       true ->
           lgis(Cmp, I + 1, L, N, X, P0, M0)
    end.

% lgis
lgis_bin_search(_Cmp, _I, Lo, Hi, _X, _M) when Lo > Hi -> Lo;
lgis_bin_search(Cmp, I, Lo, Hi, X, M) ->
    Mid = trunc(math:ceil((Lo + Hi) / 2)),
    case Cmp(array:get(array:get(Mid, M), X), array:get(I, X)) of
        true -> lgis_bin_search(Cmp, I, Mid + 1, Hi, X, M);
        false -> lgis_bin_search(Cmp, I, Lo, Mid - 1, X, M)
    end.

% lgis
rebuild_sequence(0, K, X, _P, S) -> [array:get(K, X)|S];
rebuild_sequence(L, K, X, P, S) ->
    rebuild_sequence(L - 1, array:get(K, P), X, P, [array:get(K, X)|S]).

% rear
all_reversals(L, Memo) ->
    case maps:get(L, Memo, false) of
        false ->
            V = all_reversals(L, [], sets:new()),
            {V, Memo#{L => V}};
        V -> {V, Memo}
    end.

% rear
all_reversals([], _Hist, Acc) -> Acc;
all_reversals([_], _Hist, Acc) -> Acc;
all_reversals([H|T], Hist, Acc) ->
    Acc0 = all_reversals(lists:reverse(Hist), [H|T], [], Acc),
    all_reversals(T, [H|Hist], Acc0).

all_reversals(Prefix, [], Hist, Acc) ->
    sets:add_element(Prefix ++ Hist, Acc);
all_reversals(Prefix, [H|T], Hist, Acc) ->
    Acc0 = sets:add_element(Prefix ++ Hist ++ [H|T], Acc),
    all_reversals(Prefix, T, [H|Hist], Acc0).

% rear
set_map(Fn, Set) ->
    Map = fun(X, Acc) -> sets:add_element(Fn(X), Acc) end,
    sets:fold(Map, sets:new(), Set).

% rear
reversal_distance(S1, S2, Dist, Memo0) ->
    io:format("~w: ~w~n", [Dist, maps:size(Memo0)]),
    case sets:size(sets:intersection(S1, S2)) > 0 of
        true -> Dist;
        false ->
            Fn = fun(X, {XS, M0}) ->
                         {RX, M1} = all_reversals(X, M0),
                         {sets:union(XS, RX), M1}
                 end,
            io:format("generating ns1~n"),
            {NS1, Memo1} = lists:foldl(Fn, {sets:new(), Memo0}, sets:to_list(S1)),
            io:format("generating ns2~n"),
            {NS2, Memo2} = lists:foldl(Fn, {sets:new(), Memo1}, sets:to_list(S2)),
            io:format("generated ns~n"),
            case sets:size(sets:intersection(S1, NS2)) > 0 of
                true -> Dist + 1;
                false ->
                    case sets:size(sets:intersection(NS1, S2)) > 0 of
                        true -> Dist + 1;
                        false ->
                            case sets:size(sets:intersection(NS1, NS2)) > 0 of
                                true -> Dist + 2;
                                false ->
                                    reversal_distance(NS1, NS2, Dist + 2, Memo2)
                            end
                    end
            end
    end.

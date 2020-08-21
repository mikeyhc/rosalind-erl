-module(rosalind_math).

-export([fib/2, fibd/2, iprb/3, iev/6, lia/2, perm/1,
         rosalind_rounding/1, signed_permutations/1,
         rosalind_rounding/2, complete_tree/2, longest_subsequences/1,
         fact/1, partial_permutations/2]).

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

-module(dna).

-export([count/1, to_rna/1, compliment/1, highest_gc/1, hamming/2]).

count(DNA) ->
    Inc = fun(V) -> V  + 1 end,
    lists:foldl(fun(X, Acc) -> maps:update_with(X, Inc, 1, Acc) end, #{}, DNA).

to_rna(DNA) ->
    lists:map(fun($T) -> $U; (X) -> X end, DNA).

compliment(DNA) ->
    Convert = fun($A, Acc) -> [$T|Acc];
                 ($T, Acc) -> [$A|Acc];
                 ($C, Acc) -> [$G|Acc];
                 ($G, Acc) -> [$C|Acc];
                 (10, Acc) -> Acc
              end,
    lists:foldl(Convert, [], DNA).

highest_gc(DNAs) ->
    F = fun({K, C}) -> {K, calculate_content(C)} end,
    Contents = lists:map(F, DNAs),
    [H|_] = lists:reverse(lists:keysort(2, Contents)),
    H.

calculate_content(S) ->
    F = fun($G, {GC, T}) -> {GC + 1, T + 1};
           ($C, {GC, T}) -> {GC + 1, T + 1};
           (_, {GC, T}) -> {GC, T + 1}
        end,
    {GCs, Total} = lists:foldl(F, {0, 0}, S),
    GCs / Total * 100.

hamming(S1, S2) ->
    Zipped = lists:zip(S1, S2),
    F = fun({X, X}, Acc) -> Acc;
           (_, Acc) -> Acc + 1
        end,
    lists:foldl(F, 0, Zipped).

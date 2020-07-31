-module(dna).

-export([count/1, to_rna/1, compliment/1, highest_gc/1, hamming/2,
         consensus/1, overlap/1]).

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

consensus(Fastas) ->
    Dnas = lists:map(fun({_, Dna}) -> Dna end, Fastas),
    Cols = transpose(Dnas),
    Inc = fun(X) -> X + 1 end,
    BuildCount = fun(C, Count) -> maps:update_with(C, Inc, 1, Count) end,
    RunCol = fun(Col) -> lists:foldl(BuildCount, #{}, Col) end,
    Counts = lists:map(RunCol, Cols),
    BuildConsensus = fun(X) ->
                             List = maps:to_list(X),
                             [{H, _}|_] = lists:reverse(lists:keysort(2, List)),
                             H
                     end,
    {lists:map(BuildConsensus, Counts), Counts}.

overlap(Fasta) ->
    Fn = fun({Key, DNA}) ->
                 Prefix = lists:sublist(DNA, 3),
                 Suffix = lists:reverse(lists:sublist(lists:reverse(DNA), 3)),
                 {Key, Prefix, Suffix}
         end,
    PreAndSuf = lists:map(Fn, Fasta),
    Match = fun({K, _Pre, Suf}, Acc) ->
                    [find_matches(K, Suf,  PreAndSuf)|Acc]
            end,
    Filter = fun({K, L}) -> K =/= L end,
    lists:filter(Filter, lists:flatten(lists:foldl(Match, [], PreAndSuf))).

%% internal functions

transpose([[]|_]) -> [];
transpose(M) ->
      [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

find_matches(K, Suf, List) ->
    lists:foldl(fun({L, Pre, _}, Acc) ->
                        if Pre =:= Suf -> [{K, L}|Acc];
                           true -> Acc
                        end
                end, [], List).

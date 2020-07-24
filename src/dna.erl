-module(dna).

-export([from_file/1, count/1, to_rna/1, compliment/1]).

from_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    {ok, string:trim(binary:bin_to_list(Bin))}.

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

-module(dna).

-export([from_file/1, count/1]).

from_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    {ok, binary:bin_to_list(Bin)}.

count(DNA) ->
    Inc = fun(V) -> V  + 1 end,
    lists:foldl(fun(X, Acc) -> maps:update_with(X, Inc, 1, Acc) end, #{}, DNA).

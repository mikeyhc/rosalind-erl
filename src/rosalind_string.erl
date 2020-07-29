-module(rosalind_string).

-export([substring_idx/2]).

substring_idx(Prefix, String) ->
    substring_idx(Prefix, String, 1, []).

substring_idx(_Prefix, [], _Idx, Acc) ->
    lists:reverse(Acc);
substring_idx(Prefix, L=[_|T], Idx, Acc) ->
    case lists:prefix(Prefix, L) of
        true ->
            substring_idx(Prefix, T, Idx+1, [Idx|Acc]);
        false ->
            substring_idx(Prefix, T, Idx+1, Acc)
    end.

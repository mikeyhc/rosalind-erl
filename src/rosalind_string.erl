-module(rosalind_string).

-export([substring_idx/2, all_kmers/2]).

% subs
substring_idx(Prefix, String) ->
    substring_idx(Prefix, String, 1, []).

% lexf
all_kmers(Symbols, Length) ->
    lists:sort(gen_kmers(Symbols, Length)).

%% internal functions

% subs
substring_idx(_Prefix, [], _Idx, Acc) ->
    lists:reverse(Acc);
substring_idx(Prefix, L=[_|T], Idx, Acc) ->
    case lists:prefix(Prefix, L) of
        true ->
            substring_idx(Prefix, T, Idx+1, [Idx|Acc]);
        false ->
            substring_idx(Prefix, T, Idx+1, Acc)
    end.

% lexf
gen_kmers(_L, 0) -> [[]];
gen_kmers(L, N) -> [[H|T] || H <- L, T <- gen_kmers(L, N - 1)].

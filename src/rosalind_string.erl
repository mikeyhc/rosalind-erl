-module(rosalind_string).

-export([substring_idx/2, all_kmers/2]).

% subs
substring_idx(Prefix, String) ->
    substring_idx(Prefix, String, 1, []).

% lexf
all_kmers(Symbols, Length) ->
    lists:sort(sets:to_list(all_kmers_(Symbols, Length))).

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
all_kmers_(Symbols, 1) ->
    sets:from_list(lists:map(fun(X) -> [X] end, Symbols));
all_kmers_(Symbols, N) ->
    Lower = all_kmers_(Symbols, N - 1),
    Fn = fun(Base) ->
                 F = fun(C) -> [C|Base] end,
                 lists:map(F, Symbols)
         end,
    New = lists:flatmap(Fn, sets:to_list(Lower)),
    sets:from_list(New).

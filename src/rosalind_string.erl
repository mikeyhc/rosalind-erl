-module(rosalind_string).

-export([substring_idx/2, all_kmers/2, kmp_failure/1, all_strings/2]).

% subs
substring_idx(Prefix, String) ->
    substring_idx(Prefix, String, 1, []).

% lexf
all_kmers(Symbols, Length) ->
    lists:sort(gen_kmers(Symbols, Length)).

% kmp
kmp_failure(S=[_|Str]) ->
    array:to_list(kmp_failure_(Str, array:from_list(S), 1, 0,
                               array:from_list([0]))).

% lexv
all_strings(Alphabet, N) ->
    Fn = fun(X, Acc) -> lexv_depth([X], Alphabet, N, Acc) end,
    lists:reverse(lists:foldl(Fn, [], Alphabet)).

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

% kmp
kmp_failure_([], _Str, _Pos, _Cnd, Table) -> Table;
kmp_failure_([H|T], Str, Pos, Cnd, Table) ->
    case H =:= array:get(Cnd, Str) of
        true ->
            Table0 = array:set(Pos, Cnd + 1, Table),
            kmp_failure_(T, Str, Pos + 1, Cnd + 1, Table0);
        false ->
            Cnd0 = find_cnd(Cnd, Pos, Str),
            Table0 = array:set(Pos, Cnd0, Table),
            kmp_failure_(T, Str, Pos + 1, Cnd0, Table0)
    end.

% kmp
find_cnd(-1, _Pos, _Str) -> 0;
find_cnd(Cnd, Pos, Str) ->
    case cnd_prefix(0, Cnd, Pos, Str) of
        true -> Cnd + 1;
        false -> find_cnd(Cnd - 1, Pos, Str)
    end.

% kmp
cnd_prefix(_Idx, -1, _Pos, _Str) -> true;
cnd_prefix(Idx, Cnd, Pos, Str) ->
    case array:get(Idx, Str) =:= array:get(Pos - Cnd, Str) of
        true -> cnd_prefix(Idx + 1, Cnd - 1, Pos, Str);
        false -> false
    end.

% lexv
lexv_depth(Pre, _S, 1, Acc) -> [Pre|Acc];
lexv_depth(Pre, S=[H|T], N, Acc) ->
    Acc0 = lexv_depth(Pre ++ [H], S, N - 1, [Pre|Acc]),
    lexv_breadth(Pre, S, T, N, Acc0).

% lexv
lexv_breadth(_Pre, _S, [], _N, Acc) -> Acc;
lexv_breadth(Pre, S, [H|T], N, Acc) ->
    Acc0 = lexv_depth(Pre ++ [H], S, N - 1, Acc),
    lexv_breadth(Pre, S, T, N, Acc0).

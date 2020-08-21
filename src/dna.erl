-module(dna).

-export([count/1, to_rna/1, compliment/1, highest_gc/1, hamming/2,
         consensus/1, overlap/1, shared_motif/1, open_reading_frames/1,
         reverse_palindrome/1, restriction_sites/1, spliced_motif/2,
         transition_transversion/2, shortest_superstring/1]).

% dna
count(DNA) ->
    Inc = fun(V) -> V  + 1 end,
    lists:foldl(fun(X, Acc) -> maps:update_with(X, Inc, 1, Acc) end, #{}, DNA).

% rna
to_rna(DNA) ->
    lists:map(fun($T) -> $U; (X) -> X end, DNA).

% revc
compliment(DNA) ->
    Convert = fun($A, Acc) -> [$T|Acc];
                 ($T, Acc) -> [$A|Acc];
                 ($C, Acc) -> [$G|Acc];
                 ($G, Acc) -> [$C|Acc];
                 (10, Acc) -> Acc
              end,
    lists:foldl(Convert, [], DNA).

% gc
highest_gc(DNAs) ->
    F = fun({K, C}) -> {K, calculate_content(C)} end,
    Contents = lists:map(F, DNAs),
    [H|_] = lists:reverse(lists:keysort(2, Contents)),
    H.

% hamm
hamming(S1, S2) ->
    Zipped = lists:zip(S1, S2),
    F = fun({X, X}, Acc) -> Acc;
           (_, Acc) -> Acc + 1
        end,
    lists:foldl(F, 0, Zipped).

% cons
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

% grph
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

% lcsm
shared_motif(Fastas) ->
    Lengths = lists:map(fun({_, Dna}) -> {Dna, length(Dna)} end, Fastas),
    F = fun(N={_, L}, Acc={_, SL}) ->
                if L < SL -> N;
                   true -> Acc
                end
        end,
    [H|Rest] = Lengths,
    {SDna, SLen} = lists:foldl(F, H, Rest),
    match_any_prefix(SDna, SLen, SLen, Lengths).

% orf
open_reading_frames(DNA) ->
    Compliment = compliment(DNA),
    Prots = rna:to_proteins(to_rna(DNA)) ++ rna:to_proteins(to_rna(Compliment)),
    sets:to_list(sets:from_list(Prots)).

% revp
reverse_palindrome(DNA) ->
    Compliment = compliment(DNA),
    reverse_palindrome(DNA, Compliment, 1, []).

restriction_sites(Dna) ->
    lists:reverse(find_restriction_sites(Dna, 1, length(Dna) + 1, [])).

% sseq
spliced_motif(Base, Sub) ->
    motif_indices(Base, Sub, 1, []).

% tran
transition_transversion(S1, S2) ->
    {X, Y} = tran(S1, S2, 0, 0),
    Y / X.

% long
shortest_superstring(Dna) ->
    reduce_graph(to_overlap_graph(Dna)).

%% internal functions

% gc
calculate_content(S) ->
    F = fun($G, {GC, T}) -> {GC + 1, T + 1};
           ($C, {GC, T}) -> {GC + 1, T + 1};
           (_, {GC, T}) -> {GC, T + 1}
        end,
    {GCs, Total} = lists:foldl(F, {0, 0}, S),
    GCs / Total * 100.

% cons
transpose([[]|_]) -> [];
transpose(M) ->
      [lists:map(fun hd/1, M) | transpose(lists:map(fun tl/1, M))].

% grph
find_matches(K, Suf, List) ->
    lists:foldl(fun({L, Pre, _}, Acc) ->
                        if Pre =:= Suf -> [{K, L}|Acc];
                           true -> Acc
                        end
                end, [], List).

% lcsm
is_prefix(_Prefix, _String, 0) -> true;
is_prefix([H|Prefix], [H|String], N) ->
    is_prefix(Prefix, String, N - 1);
is_prefix(_, _, _) -> false.

% lcsm
contains_string(_Prefix, PrefixLen, _String, StrLen) when StrLen < PrefixLen ->
    false;
contains_string(Prefix, PrefixLen, String=[_|Rest], StrLen) ->
    case is_prefix(Prefix, String, PrefixLen) of
        true -> true;
        false ->
            contains_string(Prefix, PrefixLen, Rest, StrLen - 1)
    end.

% lcsm
match_strings(_First, FirstLen, I, Width, _Others) when I + Width > FirstLen ->
    false;
match_strings(First=[_|Rest], FirstLen, I, Width, Others) ->
    F = fun({Str, Len}) -> not contains_string(First, Width, Str, Len) end,
    case lists:any(F, Others) of
        true ->  match_strings(Rest, FirstLen, I + 1, Width, Others);
        false -> {true, lists:sublist(First, Width)}
    end.

% lcsm
match_any_prefix(First, FirstLen, Width, Others) ->
    case match_strings(First, FirstLen, 0, Width, Others) of
        {true, Str} -> Str;
        false -> match_any_prefix(First, FirstLen, Width - 1, Others)
    end.

% revp
reverse_palindrome([], [], _Idx, Acc) -> lists:reverse(Acc);
reverse_palindrome(L1=[H|_], L2=[H|_], Idx, Acc) ->
    {T1, T2, N} = count_palindrome(L1, L2, 0),
    reverse_palindrome(T1, T2, Idx + N + 1, [{Idx, N}|Acc]);
reverse_palindrome([_|T1], [_|T2], Idx, Acc) ->
    reverse_palindrome(T1, T2, Idx + 1, Acc).

% revp
count_palindrome([H|T1], [H|T2], N) ->
    count_palindrome(T1, T2, N + 1);
count_palindrome(T1, T2, Idx) ->
    {T1, T2, Idx}.

find_restriction_sites(_, Idx, N, Acc) when N - Idx < 4 -> Acc;
find_restriction_sites(Dna=[_|T], Idx, N, Acc) ->
    ShortDna = lists:sublist(Dna, 12),
    Compliment = compliment(ShortDna),
    NewAcc = case test_prefix(ShortDna, Compliment, min(N - Idx, 12)) of
                 false -> Acc;
                 Len -> [{Idx, Len}|Acc]
             end,
    find_restriction_sites(T, Idx + 1, N, NewAcc).

test_prefix(_Dna, _Comp, Len) when Len < 4 -> false;
test_prefix(Dna, Dna, Len) -> Len;
test_prefix(Dna, [_|Comp], Len) ->
    test_prefix(lists:sublist(Dna, Len - 1), Comp, Len - 1).

% sseq
motif_indices([], _, _, _) -> throw(invalid_subsequence);
motif_indices(_, [], _, Acc) -> lists:reverse(Acc);
motif_indices([H|S], [H|T], N, Acc) ->
    motif_indices(S, T, N + 1, [N|Acc]);
motif_indices([_|S], Subs, N, Acc) ->
    motif_indices(S, Subs, N + 1, Acc).

% tran
tran([], [], X, Y) -> {X, Y};
tran([A|S], [A|T], X, Y) ->
    tran(S, T, X, Y);
tran([A|S], [B|T], X, Y) ->
    case is_transition(A, B) of
        true -> tran(S, T, X + 1, Y);
        false -> tran(S, T, X, Y + 1)
    end.

% tran
is_transition(C1, C2) ->
    is_guanine(C1) xor is_guanine(C2).

% tran
is_guanine($A) -> true;
is_guanine($G) -> true;
is_guanine(_) -> false.

% long
reduce_graph(Graph) ->
    case maps:size(Graph) of
        1 ->
            [S] = maps:keys(Graph),
           S;
        _ ->
           reduce_graph(reduce_graph_step(Graph))
    end.


% long
reduce_graph_step(Graph0) ->
    {Left, Right, Size} = largest_overlap(Graph0),
    New = lists:sublist(Left, length(Left) - Size) ++ Right,
    LeftChildren = maps:get(Left, Graph0),
    Graph1 = maps:remove(Left, Graph0),
    RightChildren = maps:get(Right, Graph1),
    Graph2 = maps:remove(Right, Graph1),
    Children = merge_children(maps:to_list(maps:remove(Right, LeftChildren)),
                              maps:to_list(maps:remove(Left, RightChildren))),
    update_graph(Left, Right, New, Graph2#{New => maps:from_list(Children)}).

% long
update_graph(Left, Right, New, Graph) ->
    Fn = fun(_Key, Value) ->
                 LV = maps:get(Left, Value, -1),
                 RV = maps:get(Right, Value, -1),
                 V0 = maps:remove(Left, Value),
                 V1 = maps:remove(Right, V0),
                 if LV + RV =:= -2 -> Value;
                    LV > RV -> V1#{New => LV};
                    true -> V1#{New => RV}
                 end
         end,
    maps:map(Fn, Graph).

% long
largest_overlap(Graph) ->
    ListMax = fun(K, N, Acc={_, M}) ->
                      if N > M -> {K, N};
                         true -> Acc
                      end
              end,
    Max = fun(K, V, Acc={_, _, M}) ->
                  {O, N} = maps:fold(ListMax, {undefined, 0}, V),
                  if N > M -> {K, O, N};
                     true -> Acc
                  end
          end,
    maps:fold(Max, {undefined, undefined, 0}, Graph).

% long
to_overlap_graph(L) ->
    lists:foldl(fun build_overlap_graph/2, #{}, mapl(fun all_matches/2, L)).

% long
build_overlap_graph({Node, Children}, Acc) ->
    lists:foldl(fun(C, A) -> build_graph_node(Node, C, A) end, Acc, Children).

% long
build_graph_node(Node, {Other, Left, Right}, A0) ->
    A1 = if Left =:= undefined -> A0;
            true ->
                 CurLeft = maps:get(Node, A0, #{}),
                 A0#{Node => CurLeft#{Other => length(Node) - Left}}
         end,
    if Right =:= undefined -> A1;
       true ->
           CurRight = maps:get(Other, A1, #{}),
           A1#{Other => CurRight#{Node => length(Other) - Right}}
    end.

% long
all_matches(Current, Rest) ->
    {Current, lists:map(fun(X) -> matches(Current, X) end, Rest)}.

% long
matches(A, B) ->
    L = matches(A, B, 0),
    R = matches(B, A, 0),
    {B, L, R}.

% long
matches([], _, _) -> undefined;
matches(A=[_|T], B, N) ->
    case is_match(A, B) of
        true -> N;
        false -> matches(T, B, N+1)
    end.

% long
is_match([], _) -> true;
is_match(_, []) -> true;
is_match([H|A], [H|B]) ->
    is_match(A, B);
is_match(_, _) -> false.

% long
mapl(Fn, L) ->
    mapl(Fn, L, []).

% long
mapl(_Fn, [], Acc) -> lists:reverse(Acc);
mapl(Fn, [H|T], Acc) ->
    mapl(Fn, T, [Fn(H, T)|Acc]).

% long
merge_children([], R) -> R;
merge_children([H={K, X}|T], R) ->
    case lists:keyfind(K, 1, R) of
        {_, Y} ->
            if X > Y ->
                   merge_children(T, lists:keystore(K, 1, R, H));
               true -> merge_children(T, R)
            end;
        false -> merge_children(T, [H|R])
    end.

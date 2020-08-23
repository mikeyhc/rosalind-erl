-module(rna).

-export([to_protein/1, codon_table/0, reverse_codon_table/0, to_proteins/1,
         rna_splice/2, rna_folding/1, non_crossing_folding/1]).

to_protein(RNA) ->
    lists:reverse(build_protein(RNA, [])).

build_protein([A, B, C|T], Acc) ->
    case codon([A, B, C]) of
        false -> Acc;
        V -> build_protein(T, [V|Acc])
    end;
build_protein(_, _Acc) -> [].

to_proteins(RNA) ->
    lists:filter(fun(L) -> L =/= [] end, build_proteins(RNA, [])).

build_proteins(L=[A, B, C|T], Acc) ->
    case codon([A, B, C]) of
        $M -> build_proteins([B, C|T], [to_protein(L)|Acc]);
        _ -> build_proteins([B, C|T], Acc)
    end;
build_proteins(_, Acc) -> Acc.

% splc
rna_splice(String, Introns) ->
    Sorted = lists:sort(fun(A, B) -> length(A) > length(B) end, Introns),
    Exons = lists:foldl(fun remove_intron/2, String, Sorted),
    rna:to_protein(Exons).

% pmch
rna_folding(Rna) ->
    Fn = fun($A, {A, C}) -> {A + 1, C};
            ($C, {A, C}) -> {A, C + 1};
            (_, Acc) -> Acc
         end,
    {A, C} = lists:foldl(Fn, {0, 0}, Rna),
    rosalind_math:fact(A) * rosalind_math:fact(C).

% cat
non_crossing_folding(Rna) ->
    {V, _Memo} = rc_with_memo(Rna, #{}),
    V.

%% internal functions

codon_table() ->
    #{"UUU" => $F, "CUU" => $L, "AUU" => $I, "GUU" => $V,
      "UUC" => $F, "CUC" => $L, "AUC" => $I, "GUC" => $V,
      "UUA" => $L, "CUA" => $L, "AUA" => $I, "GUA" => $V,
      "UUG" => $L, "CUG" => $L, "AUG" => $M, "GUG" => $V,
      "UCU" => $S, "CCU" => $P, "ACU" => $T, "GCU" => $A,
      "UCC" => $S, "CCC" => $P, "ACC" => $T, "GCC" => $A,
      "UCA" => $S, "CCA" => $P, "ACA" => $T, "GCA" => $A,
      "UCG" => $S, "CCG" => $P, "ACG" => $T, "GCG" => $A,
      "UAU" => $Y, "CAU" => $H, "AAU" => $N, "GAU" => $D,
      "UAC" => $Y, "CAC" => $H, "AAC" => $N, "GAC" => $D,
      "CAA" => $Q, "AAA" => $K, "GAA" => $E, "CAG" => $Q,
      "AAG" => $K, "GAG" => $E, "UGU" => $C, "CGU" => $R,
      "AGU" => $S, "GGU" => $G, "UGC" => $C, "CGC" => $R,
      "AGC" => $S, "GGC" => $G, "CGA" => $R, "AGA" => $R,
      "GGA" => $G, "UGG" => $W, "CGG" => $R, "AGG" => $R,
      "GGG" => $G,
      "UAA" => false,
      "UGA" => false,
      "UAG" => false
     }.

reverse_codon_table() ->
    Fn = fun({RNA, Amino}, Acc) ->
                 Current = maps:get(Amino, Acc, []),
                 Acc#{Amino => [RNA|Current]}
         end,
    lists:foldl(Fn, #{}, maps:to_list(codon_table())).

codon(Str) ->
    maps:get(Str, codon_table()).

% splc
remove_intron(Intron, String) ->
    remove_intron(Intron, String, []).

% splc
remove_intron(_Intron, [], Acc) -> lists:reverse(Acc);
remove_intron(Intron, S=[H|T], Acc) ->
    case string:prefix(S, Intron) of
        nomatch -> remove_intron(Intron, T, [H|Acc]);
        Rest -> remove_intron(Intron, Rest, Acc)
    end.

% cat
rna_catalan(_R, [], _Hist, Total, Memo) -> {Total, Memo};
rna_catalan(R, [H], [], Total, Memo) ->
    case is_bonding(R, H) of
        true -> {Total + 1, Memo};
        false -> {Total, Memo}
    end;
rna_catalan(R, [H], Hist, Total, Memo) ->
    case is_bonding(R, H) of
        true ->
            {Backward, M0} = rc_with_memo(Hist, Memo),
            {Total + Backward, M0};
        false -> {Total, Memo}
    end;
rna_catalan(R, [H,I|T], [], Total, Memo) ->
    case is_bonding(R, H) of
        true ->
            {Forward, M0} = rc_with_memo([I|T], Memo),
            rna_catalan(R, T, [I,H], Total + Forward, M0);
        false ->
            rna_catalan(R, T, [I,H], Total, Memo)
    end;
rna_catalan(R, [H,I|T], Hist, Total, Memo) ->
    case is_bonding(R, H) of
        true ->
            {Forward, M0} = rc_with_memo([I|T], Memo),
            {Backward, M1} = rc_with_memo(Hist, M0),
            rna_catalan(R, T, [I,H|Hist], Total + Forward * Backward, M1);
        false ->
            rna_catalan(R, T, [I,H|Hist], Total, Memo)
    end.

% cat
is_bonding($A, $U) -> true;
is_bonding($U, $A) -> true;
is_bonding($C, $G) -> true;
is_bonding($G, $C) -> true;
is_bonding(_, _) -> false.

% cat
rc_with_memo(Rna=[H|T], Memo) ->
    case maps:get(Rna, Memo, false) of
        false ->
            {Out, M0} = rna_catalan(H, T, [], 0, Memo),
            {Out, M0#{Rna => Out}};
        V -> {V, Memo}
    end.

-module(rna).

-export([to_protein/1, codon_table/0, reverse_codon_table/0]).

to_protein(RNA) ->
    lists:reverse(build_protein(RNA, [])).

build_protein([], Acc) -> Acc;
build_protein([A, B, C|T], Acc) ->
    case codon([A, B, C]) of
        false -> Acc;
        V -> build_protein(T, [V|Acc])
    end.

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
    #{Str := Amino} = codon_table(),
    Amino.


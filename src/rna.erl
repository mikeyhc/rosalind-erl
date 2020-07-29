-module(rna).

-export([to_protein/1]).

to_protein(RNA) ->
    lists:reverse(build_protein(RNA, [])).

build_protein([], Acc) -> Acc;
build_protein([A, B, C|T], Acc) ->
    case codon([A, B, C]) of
        false -> Acc;
        V -> build_protein(T, [V|Acc])
    end.

codon("UUU") -> $F;
codon("CUU") -> $L;
codon("AUU") -> $I;
codon("GUU") -> $V;
codon("UUC") -> $F;
codon("CUC") -> $L;
codon("AUC") -> $I;
codon("GUC") -> $V;
codon("UUA") -> $L;
codon("CUA") -> $L;
codon("AUA") -> $I;
codon("GUA") -> $V;
codon("UUG") -> $L;
codon("CUG") -> $L;
codon("AUG") -> $M;
codon("GUG") -> $V;
codon("UCU") -> $S;
codon("CCU") -> $P;
codon("ACU") -> $T;
codon("GCU") -> $A;
codon("UCC") -> $S;
codon("CCC") -> $P;
codon("ACC") -> $T;
codon("GCC") -> $A;
codon("UCA") -> $S;
codon("CCA") -> $P;
codon("ACA") -> $T;
codon("GCA") -> $A;
codon("UCG") -> $S;
codon("CCG") -> $P;
codon("ACG") -> $T;
codon("GCG") -> $A;
codon("UAU") -> $Y;
codon("CAU") -> $H;
codon("AAU") -> $N;
codon("GAU") -> $D;
codon("UAC") -> $Y;
codon("CAC") -> $H;
codon("AAC") -> $N;
codon("GAC") -> $D;
codon("UAA") -> false;
codon("CAA") -> $Q;
codon("AAA") -> $K;
codon("GAA") -> $E;
codon("UAG") -> false;
codon("CAG") -> $Q;
codon("AAG") -> $K;
codon("GAG") -> $E;
codon("UGU") -> $C;
codon("CGU") -> $R;
codon("AGU") -> $S;
codon("GGU") -> $G;
codon("UGC") -> $C;
codon("CGC") -> $R;
codon("AGC") -> $S;
codon("GGC") -> $G;
codon("UGA") -> false;
codon("CGA") -> $R;
codon("AGA") -> $R;
codon("GGA") -> $G;
codon("UGG") -> $W;
codon("CGG") -> $R;
codon("AGG") -> $R;
codon("GGG") -> $G.

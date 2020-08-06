-module(rosalind_prot).

-export([protein_motifs/1, infer_mrna/1]).

% mprt
protein_motifs(ProteinIds) ->
    lists:map(fun to_motif_locations/1, ProteinIds).

% mrna
infer_mrna(Protein) ->
    Table = rna:reverse_codon_table(),
    Lens = lists:map(fun(X) -> length(maps:get(X, Table)) end, Protein),
    Fn = fun(X, Acc) -> X * Acc rem 1_000_000  end,
    lists:foldl(Fn, 1, Lens) * 3 rem 1_000_000. % 3 for stop codon


%% internal functions

% mprt
to_motif_locations(ProteinId) ->
    Protein = fetch_protein(ProteinId),
    {ProteinId, motif_locations(Protein, 1, [])}.

% mprt
fetch_protein(ProteinId) ->
    URI = "/uniprot/" ++ ProteinId ++ ".fasta",
    Bin = rosalind_http:get("www.uniprot.org", URI),
    [{_, Protein}] = fasta:to_fasta(binary:bin_to_list(Bin)),
    Protein.

% mprt
is_nglyo([$N, P0, ST, P1|_]) when P0 =/= $P andalso
                                  P1 =/= $P andalso
                                  (ST =:= $S orelse ST =:= $T) ->
    true;
is_nglyo(_) -> false.

% mpt
motif_locations([_, _, _], _Idx, Acc) -> lists:reverse(Acc);
motif_locations(Input=[_|T], Idx, Acc) ->
    case is_nglyo(Input) of
        true -> motif_locations(T, Idx + 1, [Idx|Acc]);
        false -> motif_locations(T, Idx + 1, Acc)
    end.

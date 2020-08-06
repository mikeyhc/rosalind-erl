-module(rosalind_file).

-export([multiline_file/1, fasta_file/1]).

multiline_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    {ok, lists:filter(fun(X) -> X =/= [] end,
                      string:split(binary:bin_to_list(Bin), "\n", all))}.

fasta_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, fasta:to_fasta(binary:bin_to_list(Bin))}.

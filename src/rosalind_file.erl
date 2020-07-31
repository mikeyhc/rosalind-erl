-module(rosalind_file).

-export([multiline_file/1, fasta_file/1]).

multiline_file(Path) ->
    {ok, Bin} = file:read_file(Path),
    {ok, lists:filter(fun(X) -> X =/= [] end,
                      string:split(binary:bin_to_list(Bin), "\n", all))}.

fasta_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    {ok, to_fasta(string:split(binary:bin_to_list(Bin), "\n", all))}.

to_fasta(Lines) ->
    to_fasta(Lines, []).

to_fasta([], Fasta) ->
    lists:map(fun to_dna_string/1, Fasta);
to_fasta([[$>|Name]|T], Acc) ->
    to_fasta(T, [{Name, []}|Acc]);
to_fasta([Line|T], [{Name, Lines}|Acc]) ->
    to_fasta(T, [{Name, [Line|Lines]}|Acc]).

to_dna_string({Key, Lines}) ->
    {Key, lists:concat(lists:reverse(Lines))}.

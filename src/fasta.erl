-module(fasta).

-export([read_file/1]).

read_file(Filename) ->
    {ok, Bin} = file:read_file(Filename),
    to_fasta(string:split(binary:bin_to_list(Bin), "\n", all)).

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

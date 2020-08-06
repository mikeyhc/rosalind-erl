-module(fasta).

-export([to_fasta/1]).

to_fasta(Input) ->
    to_fasta(string:split(Input, "\n", all), []).

to_fasta([], Fasta) ->
    lists:map(fun to_dna_string/1, Fasta);
to_fasta([[$>|Name]|T], Acc) ->
    to_fasta(T, [{Name, []}|Acc]);
to_fasta([Line|T], [{Name, Lines}|Acc]) ->
    to_fasta(T, [{Name, [Line|Lines]}|Acc]).

to_dna_string({Key, Lines}) ->
    {Key, lists:concat(lists:reverse(Lines))}.

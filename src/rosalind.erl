-module(rosalind).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    io:format("no group provided~n"),
    usage(),
    erlang:halt(1);
main([Group|Args]) ->
    case Group of
        "dna" -> dna_commands(Args);
        _ ->
            %% TODO print to stderr
            io:format("command ~s unknown~n", [Group]),
            usage(),
            erlang:halt(1)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

dna_commands([]) ->
    %% TODO stderr
    io:format("no command provided~n"),
    dna_usage(),
    erlang:halt(1);
dna_commands([Command|Args]) ->
    case Command of
        "count" -> dna_count(Args);
        "rna" -> dna_to_rna(Args);
        "compliment" -> dna_compliment(Args);
        _ ->
            io:format("unknown command ~s~n", [Command]),
            dna_count_usage(),
            erlang:halt(1)
    end.

usage() ->
    io:format("usage: ~s GROUP COMMAND~n~n", [escript:script_name()]),
    io:format("group~n"),
    io:format("  dna     operations on dna~n~n"),
    io:format("dna commands~n"),
    io:format("  count       count all the symbols in the string~n"),
    io:format("  rna         convert DNA to RNA~n"),
    io:format("  compliment  conver the DNA to its compliment~n").

dna_usage() ->
    io:format("usage: ~s dna COMMAND~n~n", [escript:script_name()]),
    io:format("commands~n"),
    io:format("  count       count all the symbols in the string~n"),
    io:format("  rna         convert DNA to RNA~n"),
    io:format("  compliment  conver the DNA to its compliment~n").

dna_count_usage() ->
    io:format("usage: ~s dna count <path>~n", [escript:script_name()]).

dna_to_rna_usage() ->
    io:format("usage: ~s dna rna <path>~n", [escript:script_name()]).

dna_compliment_usage() ->
    io:format("usage: ~s dna compliment <path>~n", [escript:script_name()]).

dna_count([Path]) ->
    {ok, DNA} = dna:from_file(Path),
    #{$A := As, $C := Cs, $G := Gs, $T := Ts} = dna:count(DNA),
    io:format("A: ~p; C: ~p; G: ~p; T: ~p~n", [As, Cs, Gs, Ts]);
dna_count(_) ->
    dna_count_usage(),
    erlang:halt(1).

dna_to_rna([Path]) ->
    {ok, DNA} = dna:from_file(Path),
    io:format("~s~n", [dna:to_rna(DNA)]);
dna_to_rna(_) ->
    dna_to_rna_usage(),
    erlang:halt(1).

dna_compliment([Path]) ->
    {ok, DNA} = dna:from_file(Path),
    io:format("~s~n", [dna:compliment(DNA)]);
dna_compliment(_) ->
    dna_compliment_usage(),
    erlang:halt(1).

-module(rosalind).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    io:format("no group provided~n"),
    usage(command_list()),
    erlang:halt(1);
main([Group|Args]) ->
    case maps:get(Group, command_list(), false) of
        false ->
            %% TODO print to stderr
            io:format("command ~s unknown~n", [Group]),
            usage(command_list()),
            erlang:halt(1);
        Command ->
            handle_command(Command, Group, Args)
    end.

%%====================================================================
%% Internal functions
%%====================================================================

command_list() ->
    #{"dna" =>
      #{description => "operations on DNA",
        commands =>
        #{"count" =>
          #{description => "count all the symbols in the string",
            arg => "PATH",
            function => fun dna_count/1},
          "rna" =>
          #{description => "convert DNA to RNA",
            arg => "PATH",
            function => fun dna_to_rna/1},
          "compliment" =>
          #{description => "convert the DNA to its compliment",
            arg => "PATH",
            function => fun dna_compliment/1},
          "gc" =>
          #{description => "determine the string with the highest GC content",
            arg => "PATH",
            function => fun gc/1},
          "hamm" =>
          #{description => "calculate the hamming distance between two strings",
            arg => "PATH",
            function => fun hamm/1},
          "cons" =>
          #{description => "determine the consensus string for FASTA strings",
            arg => "PATH",
            function => fun cons/1},
          "grph" =>
          #{description => "map a set of fasta strings to a graph",
            arg => "PATH",
            function => fun grph/1}}},
      "math" =>
      #{description => "math based functions",
        commands =>
        #{"fib" =>
          #{description => "calculate the fibonacci sequence",
            arg => "K M",
            function => fun fib/1},
          "iprb" =>
          #{description => "calculate the probably of a homozygous dominant",
            arg => "K M N",
            function => fun iprb/1},
          "fibd" =>
          #{description => "calculate a decaying fibonacci sequence",
            arg => "N M",
            function => fun fibd/1}}},
      "rna" =>
      #{description => "operations on RNA",
        commands =>
        #{"prot" =>
          #{description => "convert an RNA to a protein",
            arg => "PATH",
            function => fun prot/1}}},
      "string" =>
      #{description => "operations on strings",
        commands =>
        #{"subs" =>
          #{description => "find all occurances of the prefix in the string",
            arg => "PATH",
            function => fun subs/1}}}}.

handle_command(Commands, Group, []) ->
    %% TODO stderr
    io:format("no command provided~n"),
    command_usage(Group, Commands),
    erlang:halt(1);
handle_command(#{commands := Commands}, Group, [Command|Args]) ->
    case maps:get(Command, Commands, false) of
        false ->
            io:format("unknown command ~s~n", [Command]),
            command_usage(Group, Commands),
            erlang:halt(1);
        Obj ->
            #{arg := ExpectedArgs} = Obj,
            handle_subcommand(Group, Command, Obj, Args, ExpectedArgs)
    end.

handle_subcommand(Group, Command, _, [], ExpectedArgs) ->
    io:format("usage: ~s ~s ~s ~s~n",
              [escript:script_name(), Group, Command, ExpectedArgs]);
handle_subcommand(_Group, _Command, #{function := F}, Arg, _ExpectedArgs) ->
    F(Arg).

%% usage functions

command_usage(Group, #{commands := Command}) ->
    io:format("usage: ~s ~s COMMAND~n~n", [escript:script_name(), Group]),
    io:format("commands~n"),
    F = fun({Key, #{description := Desc}}) ->
                io:format("  ~s  ~s~n", [string:left(Key, 10), Desc])
        end,
    lists:foreach(F, maps:to_list(Command)).

usage(Commands) ->
    io:format("usage: ~s GROUP COMMAND~n~n", [escript:script_name()]),
    io:format("groups~n"),
    PrintCmd = fun({Key, #{description := Desc}}) ->
                       io:format("    ~s  ~s~n", [string:left(Key, 10), Desc])
               end,
    lists:foreach(PrintCmd, maps:to_list(Commands)),
    io:format("~n"),
    lists:foreach(fun print_command/1, maps:to_list(Commands)).

print_command({Key, #{commands := Commands}}) ->
    io:format("~s commands~n", [Key]),
    F = fun({Cmd, #{description := Desc}}) ->
                io:format("    ~s  ~s~n", [string:left(Cmd, 10), Desc])
        end,
    lists:foreach(F, maps:to_list(Commands)),
    io:format("~n").

%% dna functions

dna_count([Path]) ->
    {ok, [DNA]} = rosalind_file:multiline_file(Path),
    #{$A := As, $C := Cs, $G := Gs, $T := Ts} = dna:count(DNA),
    io:format("A: ~p; C: ~p; G: ~p; T: ~p~n", [As, Cs, Gs, Ts]).

dna_to_rna([Path]) ->
    {ok, [DNA]} = rosalind_file:multiline_file(Path),
    io:format("~s~n", [dna:to_rna(DNA)]).

dna_compliment([Path]) ->
    {ok, [DNA]} = rosalind_file:multiline_file(Path),
    io:format("~s~n", [dna:compliment(DNA)]).

gc([Filename]) ->
    {ok, Fasta} = rosalind_file:fasta_file(Filename),
    {Name, Content} = dna:highest_gc(Fasta),
    io:format("~s~n~9.6f~n", [Name, Content]).

hamm([Path]) ->
    {ok, [S1, S2]} = rosalind_file:multiline_file(Path),
    io:format("~w~n", [dna:hamming(S1, S2)]).

cons([Path]) ->
    {ok, Fasta} = rosalind_file:fasta_file(Path),
    {Consensus, Counts} = dna:consensus(Fasta),
    io:format("~s~n", [Consensus]),
    lists:foreach(fun(C) -> cons_print_line(C, Counts) end, "ACGT").

cons_print_line(Char, Counts) ->
    Cs = lists:map(fun(M) -> integer_to_list(maps:get(Char, M, 0)) end, Counts),
    io:format("~c: ~s~n", [Char, string:join(Cs, " ")]).

grph([Path]) ->
    {ok, Fasta} = rosalind_file:fasta_file(Path),
    F = fun({A, B}) -> io:format("~s ~s~n", [A, B]) end,
    lists:foreach(F, dna:overlap(Fasta)).

%% rna functions

prot([Path]) ->
    {ok, [S1]} = rosalind_file:multiline_file(Path),
    io:format("~s~n", [rna:to_protein(S1)]).

%% math functions

fib([N, K]) ->
    io:format("~w~n",
              [rosalind_math:fib(list_to_integer(N), list_to_integer(K))]).

iprb([K, M, N]) ->
    io:format("~f~n",
              [rosalind_math:iprb(list_to_integer(K), list_to_integer(M),
                                  list_to_integer(N))]).

fibd([M, N]) ->
    io:format("~w~n", [rosalind_math:fibd(list_to_integer(M),
                                          list_to_integer(N))]).

%% string functions

subs([Path]) ->
    {ok, [String, Prefix]} = rosalind_file:multiline_file(Path),
    StringIdxs = lists:map(fun integer_to_list/1,
                           rosalind_string:substring_idx(Prefix, String)),
    io:format("~s~n", [string:join(StringIdxs, " ")]).

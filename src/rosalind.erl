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
            function => fun dna_count/1},
          "rna" =>
          #{description => "convert DNA to RNA",
            function => fun dna_to_rna/1},
          "compliment" =>
          #{description => "convert the DNA to its compliment",
            function => fun dna_compliment/1}}}}.

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
            handle_subcommand(Group, Command, Obj, Args)
    end.

handle_subcommand(Group, Command, _, []) ->
    io:format("usage: ~s ~s ~s <path>~n",
              [escript:script_name(), Group, Command]);
handle_subcommand(_Group, _Command, #{function := F}, [Arg]) ->
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

dna_count(Path) ->
    {ok, DNA} = dna:from_file(Path),
    #{$A := As, $C := Cs, $G := Gs, $T := Ts} = dna:count(DNA),
    io:format("A: ~p; C: ~p; G: ~p; T: ~p~n", [As, Cs, Gs, Ts]).

dna_to_rna(Path) ->
    {ok, DNA} = dna:from_file(Path),
    io:format("~s~n", [dna:to_rna(DNA)]).

dna_compliment(Path) ->
    {ok, DNA} = dna:from_file(Path),
    io:format("~s~n", [dna:compliment(DNA)]).

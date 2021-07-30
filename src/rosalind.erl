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
            function => fun grph/1},
         "lcsm" =>
         #{description => "determine the longest shared motif in a set of "
                          "FASTA strings",
           arg => "PATH",
           function => fun lcsm/1},
         "orf" =>
         #{description => "find all open reading frames in the FASTA string",
           arg => "PATH",
           function => fun orf/1},
         "revp" =>
         #{description => "find all the restriction sites in a DNA sting",
           arg => "PATH",
           function => fun revp/1},
         "splc" =>
         #{description => "create a protein from the DNA exons after removing "
                          "the introns.",
           arg => "PATH",
           function => fun splc/1},
         "sseq" =>
         #{description => "determine the indices of the substrings",
           arg => "PATH",
           function => fun sseq/1},
         "tran" =>
         #{description => "find the transition/transversion ratio",
           arg => "PATH",
           function => fun tran/1},
         "long" =>
         #{description => "determine the shortest superstring",
           arg => "PATH",
           function => fun long/1},
         "prob" =>
         #{description => "determine the likelyhood of the provided string "
                          "occuring with the given GC probabilities",
           arg => "PATH",
           function => fun prob/1},
         "corr" =>
         #{description => "attempt to correct read errors in DNA sequences",
           arg => "PATH",
           function => fun corr/1},
         "kmer" =>
         #{description => "count all the occurances of the 4-mers in the given "
                          "dna",
           arg => "PATH",
           function => fun kmer/1},
         "kmp" =>
         #{description => "create a KMP failure function for the given DNA",
           arg => "PATH",
           function => fun kmp/1},
         "pdst" =>
         #{description => "create a difference matrix for the given DNA",
           arg => "PATH",
           function => fun pdst/1},
         "rstr" =>
         #{description => "calculate the possibility of the random string "
                          "occuring",
           arg => "PATH",
           function => fun rstr/1},
         "lcsq" =>
         #{description => "determine the longest common subsequence",
           arg => "PATH",
           function => fun lcsq/1}}},
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
            function => fun fibd/1},
          "iev" =>
          #{description => "caluclate expected offspring",
            arg => "A B C D E F",
            function => fun iev/1},
         "lia" =>
         #{description => "determine the likelyhood of N AaBb offspring by "
                          "generation K",
           arg => "K N",
           function => fun lia/1},
         "perm" =>
         #{description => "produce all the permutations from 1 to N",
           arg => "N",
           function => fun perm/1},
         "sign" =>
         #{description => "produce all the signed permutations from 1 to N",
           arg => "N",
           function => fun sign/1},
         "tree" =>
         #{description => "count the edges required for a full tree",
           arg => "PATH",
           function => fun tree/1},
         "lgis" =>
         #{description => "find the longest subsequences",
           arg => "PATH",
           function => fun lgis/1},
         "pper" =>
         #{description => "calculate the number of partial permutations",
           arg => "N K",
           function => fun pper/1},
         "inod" =>
         #{description => "determine the number of internal nodes in a binary "
                          "tree",
           arg => "N",
           function => fun inod/1},
         "sset" =>
         #{description => "count the number of subsets for a give set size",
           arg => "N",
           function => fun sset/1},
         "rear" =>
         #{description => "determine the number of steps to unreverse the "
                          "given strings",
           arg => "PATH",
           function => fun rear/1}}},
      "rna" =>
      #{description => "operations on RNA",
        commands =>
        #{"prot" =>
          #{description => "convert an RNA to a protein",
            arg => "PATH",
            function => fun prot/1},
         "pmch" =>
         #{description => "determine the total number of RNA foldings",
           arg => "PATH",
           function => fun pmch/1},
         "cat" =>
         #{description => "determine the number of non-crossing RNA foldings",
           arg => "PATH",
           function => fun cat/1},
         "mmch" =>
         #{description => "determine the number of maximum matches the RNA is "
                          "capable of",
           arg => "PATH",
           function => fun mmch/1}}},
      "string" =>
      #{description => "operations on strings",
        commands =>
        #{"subs" =>
          #{description => "find all occurances of the prefix in the string",
            arg => "PATH",
            function => fun subs/1},
          "lexf" =>
          #{description => "created a sorted list of length N of sorted "
                           "alphabet A",
            arg => "PATH",
            function => fun lexf/1},
          "lexv" =>
          #{description => "generate all strings of length N from alphabet A",
            arg => "PATH",
            function => fun lexv/1}}},
     "prot" =>
     #{description => "operations on proteins",
      commands =>
      #{"mprt" =>
        #{description => "find the N-glycosylation motifs in the provided "
                         "strings",
          arg => "PATH",
          function => fun mprt/1},
        "mrna" =>
        #{description => "calculate the number of possible RNA strings that "
          "could have led to this protein string",
          arg => "PATH",
          function => fun mrna/1},
       "prtm" =>
       #{description => "calculate the mass of this protein",
         arg => "PATH",
         function => fun prtm/1}}}}.

handle_command(#{commands := Commands}, Group, []) ->
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

command_usage(Group, Command) ->
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

lcsm([Path]) ->
    {ok, Fasta} = rosalind_file:fasta_file(Path),
    io:format("~s~n", [dna:shared_motif(Fasta)]).

orf([Path]) ->
    {ok, [{_, Dna}]} = rosalind_file:fasta_file(Path),
    Fn = fun(ORF) -> io:format("~s~n", [ORF]) end,
    lists:foreach(Fn, dna:open_reading_frames(Dna)).

revp([Path]) ->
    {ok, [{_, Dna}]} = rosalind_file:fasta_file(Path),
    Sites = dna:restriction_sites(Dna),
    lists:foreach(fun({P, L}) -> io:format("~w ~w~n", [P, L]) end, Sites).

long([Path]) ->
    {ok, Fasta} = rosalind_file:fasta_file(Path),
    Dna = lists:map(fun({_, X}) -> X end, Fasta),
    io:format("~s~n", [dna:shortest_superstring(Dna)]).

splc([Path]) ->
    {ok, Fastas} = rosalind_file:fasta_file(Path),
    [Base|Rest] = lists:map(fun({_, Dna}) -> dna:to_rna(Dna) end, Fastas),
    Protein = rna:rna_splice(Base, Rest),
    io:format("~s~n", [Protein]).

sseq([Path]) ->
    {ok, [{_, Base}, {_, Sub}]} = rosalind_file:fasta_file(Path),
    Idxs = dna:spliced_motif(Base, Sub),
    SIdxs = string:join(lists:map(fun integer_to_list/1, Idxs), " "),
    io:format("~s~n", [SIdxs]).

tran([Path]) ->
    {ok, [{_, S1}, {_, S2}]} = rosalind_file:fasta_file(Path),
    Ratio = dna:transition_transversion(S1, S2),
    io:format("~13.11f~n", [rosalind_math:rosalind_rounding(Ratio, 11)]).

prob([Path]) ->
    {ok, [DNA, StrProbs]} = rosalind_file:multiline_file(Path),
    InProbs = lists:map(fun list_to_float/1, string:split(StrProbs, " ", all)),
    OutProbs = dna:random_strings(DNA, InProbs),
    Render = fun(X) ->
                     io:format("~8.3f ", [rosalind_math:rosalind_rounding(X)])
             end,
    lists:foreach(Render, OutProbs),
    io:format("~n").

corr([Path]) ->
    {ok, Fasta} = rosalind_file:fasta_file(Path),
    Dna = lists:map(fun({_, D}) -> D end, Fasta),
    Corrs = dna:corrections(Dna),
    lists:foreach(fun({D, C}) -> io:format("~s->~s~n", [D, C]) end, Corrs).

kmer([Path]) ->
    {ok, [{_, Dna}]} = rosalind_file:fasta_file(Path),
    Counts = lists:map(fun integer_to_list/1, dna:four_mer_composition(Dna)),
    io:format("~s~n", [string:join(Counts, " ")]).

kmp([Path]) ->
    {ok, [{_, Dna}]} = rosalind_file:fasta_file(Path),
    Failure = lists:map(fun integer_to_list/1,
                        rosalind_string:kmp_failure(Dna)),
    io:format("~s~n", [string:join(Failure, " ")]).

pdst([Path]) ->
    {ok, Fasta} = rosalind_file:fasta_file(Path),
    Dna = lists:map(fun({_, D}) -> D end, Fasta),
    Matrix = dna:distance_matrix(Dna),
    RFn = fun(V) ->
                  io:format("~8.6f ", [rosalind_math:rosalind_rounding(V, 8)])
          end,
    Render = fun(L) ->
                     lists:foreach(RFn, L),
                     io:format("~n")
             end,
    lists:foreach(Render, Matrix).

rstr([Path]) ->
    {ok, [Nums, Dna]} = rosalind_file:multiline_file(Path),
    [SN, SX] = string:split(Nums, " "),
    Prob = dna:random_string_chance(list_to_integer(SN),
                                    list_to_float(SX),
                                    Dna),
    io:format("~5.3f~n", [rosalind_math:rosalind_rounding(Prob)]).

lcsq([Path]) ->
    {ok, [{_, A}, {_, B}]} = rosalind_file:fasta_file(Path),
    io:format("~s~n", [dna:spliced_shared_motif(A, B)]).

%% rna functions

prot([Path]) ->
    {ok, [S1]} = rosalind_file:multiline_file(Path),
    io:format("~s~n", [rna:to_protein(S1)]).

pmch([Path]) ->
    {ok, [{_, RNA}]} = rosalind_file:fasta_file(Path),
    io:format("~p~n", [rna:rna_folding(RNA)]).

cat([Path]) ->
    {ok, [{_, RNA}]} = rosalind_file:fasta_file(Path),
    io:format("~p~n", [rna:non_crossing_folding(RNA) rem 1_000_000]).

mmch([Path]) ->
    {ok, [{_, RNA}]} = rosalind_file:fasta_file(Path),
    io:format("~w~n", [rna:max_matching(RNA)]).

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

iev(L=[_, _, _, _, _, _]) ->
    Nums = lists:map(fun list_to_integer/1, L),
    io:format("~w~n", [apply(rosalind_math, iev, Nums)]).

lia(L=[_, _]) ->
    Nums = lists:map(fun list_to_integer/1, L),
    io:format("~5.3f~n", [apply(rosalind_math, lia, Nums)]).

perm([N]) ->
    Perms = rosalind_math:perm(list_to_integer(N)),
    io:format("~w~n", [length(Perms)]),
    Fn = fun(L) ->
                 Ns = lists:map(fun integer_to_list/1, L),
                 io:format("~s~n", [string:join(Ns, " ")])
         end,
    lists:foreach(Fn, Perms).

tree([Path]) ->
    {ok, [N|Edges]} = rosalind_file:multiline_file(Path),
    Fn = fun(S) ->
                 [A, B] = string:split(S, " "),
                 {list_to_integer(A), list_to_integer(B)}
         end,
    Graph = lists:map(Fn, Edges),
    Count = rosalind_math:complete_tree(list_to_integer(N), Graph),
    io:format("~w~n", [Count]).

sign([N]) ->
    Perms = rosalind_math:signed_permutations(list_to_integer(N)),
    io:format("~w~n", [length(Perms)]),
    Fn = fun(L) ->
                 Ns = lists:map(fun integer_to_list/1, L),
                 io:format("~s~n", [string:join(Ns, " ")])
         end,
    lists:foreach(Fn, Perms).

lgis([Path]) ->
    {ok, [_|Input]} = rosalind_file:multiline_file(Path),
    Parts = lists:map(fun list_to_integer/1, string:split(Input, " ", all)),
    {Inc, Dec} = rosalind_math:longest_subsequences(Parts),
    io:format("~s~n",
              [string:join(lists:map(fun integer_to_list/1, Inc), " ")]),
    io:format("~s~n",
              [string:join(lists:map(fun integer_to_list/1, Dec), " ")]).

pper([N, K]) ->
    Partials = rosalind_math:partial_permutations(list_to_integer(N),
                                                  list_to_integer(K)),
    io:format("~w~n", [Partials rem 1_000_000]).

inod([N]) ->
    io:format("~w~n", [rosalind_math:tree_nodes(list_to_integer(N))]).

sset([N]) ->
    io:format("~w~n", [rosalind_math:subset_count(list_to_integer(N))
                       rem 1_000_000]).

rear([Path]) ->
    {ok, Lines} = rosalind_file:multiline_file(Path),
    Parts = lists:map(fun(S) -> string:split(S, " ", all) end, Lines),
    Nums = lists:map(fun(N) -> lists:map(fun list_to_integer/1, N) end,
                     Parts),
    SNums = lists:map(fun integer_to_list/1, rear_iter(Nums, [])),
    io:format("~s~n", [string:join(SNums, " ")]).

rear_iter([], Acc) -> lists:reverse(Acc);
rear_iter([H,I|T], Acc) ->
    rear_iter(T, [rosalind_math:count_reversals(H, I)|Acc]).

%% string functions

subs([Path]) ->
    {ok, [String, Prefix]} = rosalind_file:multiline_file(Path),
    StringIdxs = lists:map(fun integer_to_list/1,
                           rosalind_string:substring_idx(Prefix, String)),
    io:format("~s~n", [string:join(StringIdxs, " ")]).

lexf([Path]) ->
    {ok, [Alphabet, SNum]} = rosalind_file:multiline_file(Path),
    N = list_to_integer(SNum),
    Dense = lists:filter(fun(X) -> X =/= $  end, Alphabet),
    Lex = rosalind_string:all_kmers(Dense, N),
    lists:foreach(fun(S) -> io:format("~s~n", [S]) end, Lex).

lexv([Path]) ->
    {ok, [Alphabet, SNum]} = rosalind_file:multiline_file(Path),
    N = list_to_integer(SNum),
    Dense = lists:filter(fun(X) -> X =/= $  end, Alphabet),
    Lex = rosalind_string:all_strings(Dense, N),
    lists:foreach(fun(S) -> io:format("~s~n", [S]) end, Lex).

%% protein functions

mprt([Path]) ->
    application:ensure_all_started(gun),
    {ok, Strings} = rosalind_file:multiline_file(Path),
    ProteinLocations = rosalind_prot:protein_motifs(Strings),
    Filtered = lists:filter(fun({_, []}) -> false;
                               ({_, _}) -> true
                            end, ProteinLocations),
    Fn = fun({Id, Locations}) ->
                 io:format("~s~n", [Id]),
                 Pos = lists:map(fun integer_to_list/1, Locations),
                 io:format("~s~n", [string:join(Pos, " ")])
         end,
    lists:foreach(Fn, Filtered).

mrna([Path]) ->
    {ok, [Protein]} = rosalind_file:multiline_file(Path),
    io:format("~w~n", [rosalind_prot:infer_mrna(Protein)]).

prtm([Path]) ->
    {ok, [Protein]} = rosalind_file:multiline_file(Path),
    Mass = rosalind_prot:protein_mass(Protein),
    io:format("~w~n", [rosalind_math:rosalind_rounding(Mass)]).

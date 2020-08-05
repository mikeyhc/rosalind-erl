-module(rosalind_math).

-export([fib/2, fibd/2, iprb/3, iev/6, lia/2]).

fib(N, K) ->
    fib(N, K, 1, 0).

fib(1, _K, Child, Adult) -> Child + Adult;
fib(N, K, Child, Adult) ->
    fib(N - 1, K, Adult * K, Adult + Child).

fibd(N, M) ->
    fibd(1, N, M, 1, 0, []).

fibd(N, N, _M, Child, Adult, _History)  ->
    Child + Adult;
fibd(I, N, M, Child, Adult, History) when I >= M ->
    fibd(I + 1, N, M, Adult, Child + Adult - lists:nth(M - 1, History),
         [Child|History]);
fibd(I, N, M, Child, Adult, History) ->
    fibd(I + 1, N, M, Adult, Child + Adult, [Child|History]).

iprb(K, M, N) ->
    Total = K + M + N,
    KPercent = K / Total,
    MInitial = M * 0.5 / Total,
    NInitial = N / Total,

    MK = MInitial * K / (Total - 1),
    MM = MInitial * (M - 1) * 0.5 / (Total - 1),
    NK = NInitial * K / (Total - 1),
    NM = NInitial * M * 0.5 / (Total - 1),

    MInitial + KPercent + MK + MM + NK + NM.

iev(A, B, C, D, E, _F) ->
    (A + B + C) * 2 + D * 1.5 + E.

lia(K, N) ->
    Children = round(math:pow(2, K)),
    Fn = fun(X) ->
                 BC = binomial_coefficient(Children, X),
                 Out = math:pow(0.75, Children - X),
                 In = math:pow(0.25, X),
                 BC * In * Out
         end,
    rosalind_rouding(1 - lists:sum(lists:map(Fn, lists:seq(0, N - 1)))).

%% helper methods

% lia
fact(N) -> fact(N, 1).

fact(0, Acc) -> Acc;
fact(N, Acc) ->
    fact(N - 1, Acc * N).

% lia
binomial_coefficient(N, K) ->
    fact(N) / (fact(K) * fact(N - K)).

% lia
rosalind_rouding(N) ->
    round(N * 1000) / 1000.

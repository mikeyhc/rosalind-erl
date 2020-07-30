-module(rosalind_math).

-export([fib/2, fibd/2, iprb/3]).

fib(N, K) ->
    fib(N, K, 1, 0).

fib(1, _K, Child, Adult) -> Child + Adult;
fib(N, K, Child, Adult) ->
    fib(N - 1, K, Adult * K, Adult + Child).

fibd(N, M) ->
    fibd(1, N, M, 1, 0, []).

fibd(N, N, M, Child, Adult, History)  ->
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

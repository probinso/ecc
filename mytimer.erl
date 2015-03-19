-module(mytimer).
-export([tcn/2]).

-import(encrypt,[encrypt_file/4,decrypt_file/4]).
-import(encrypt_p,[encrypt_file/5,decrypt_file/5]).
-export([go/4,do/1,do/0]).

do()->
    do([1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]).
do(Threads)->do(Threads,[8+1,16+1,24+1,32+1,64+1,128+1,192-1]).
% do performs encryption and decryption over each pair of thread / bit-size
%   do is intended for gathering statistically significant information
%   about the speed of this process. functions are wrapped up and passed 
%   to TCN which takes in a process and a number of times to perform . 
do(Threads,Bits)-> % the sample file 'KandiCaps' can be replaced 
    Master = fun(T,B)-> mytimer:go(T,B,"KandiCaps",100) end,
    
    F = fun(T) -> fun(B) -> {T,B,Master(T,B)} end end,
    
    G = fun(X) -> lists:map(X,Bits) end,	
        
    K = lists:map(F,Threads),
    
    Results = lists:map(G,K),

    {ok,FStream} = file:open("stats",write),
    
    io:format(FStream,"~s, ~s, ~s \n",["Threads","Bits","Time"]),
    
    GG = 
      fun(X)-> 
        {TThreads,BBits,{TTime}} = X, io:format(FStream,"~w, ~w, ~w \n", 
        [TThreads,BBits,TTime]) 
      end,

    FF = fun(L) -> lists:map(GG,L) end,
    
    lists:map(FF,Results).

% It is important to note that reading from the file, and 
%   writing to the next file, are kept seperate from the 
%   timed functions. The TCN function takes in our behavior 
%   and intended repititions for each task, then returns 
%   the execution time of the significant process. 
go(1,Bits,FileIn,Reps)->
    io:fwrite("Start One Thread \n",[]),

    P = encrypt:serverSet(Bits),
    {N,NP} = encrypt:getMP(P),
    {M,MP} = encrypt:getMP(P),

    {ok,B_In} = file:read_file(FileIn),

    FE = fun() -> encrypt:encrypt_file(B_In,N,MP,Bits) end,
    FD = fun(L) -> encrypt:decrypt_file(L,M,NP,Bits) end,
    FTest = fun() -> FD(FE()) end,

    io:fwrite("Linear ~w \n",[Bits]),
    {tcn(FTest,Reps)};

go(Threads,Bits,FileIn,Reps)->
    io:fwrite("Start \n",[]),
    
    P = encrypt:serverSet(Bits),
    {N,NP} = encrypt:getMP(P),
    {M,MP} = encrypt:getMP(P),

    {ok,B_In} = file:read_file(FileIn),

    GE = fun() -> encrypt_p:encrypt_file(B_In,N,MP,Bits,Threads) end,
    GD = fun(L) -> encrypt_p:decrypt_file(L,M,NP,Bits,Threads) end,
    GTest = fun() -> GD(GE()) end,

    io:fwrite("Parallel :: ~w :: ~w \n",[Threads,Bits]),    
    {tcn(GTest,Reps)}.


% the TCN functions are used as timers.
tcn2(_T,_F,0) -> ok;
tcn2(T,F,N) ->
    _ = F(),
    T(T,F,N-1).

tcn(F,N)->
    B = now(),
    FF = fun(X,Y,Z) -> tcn2(X,Y,Z) end,
    tcn2(FF,F,N),
    A = now(),
    timer:now_diff(A,B)/N.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% TCN2 = fun(T,F,0) -> ok; (T,F,N) -> F(), T(T,F,N-1) end.
% TCN = fun(F,N) -> B=now(), TCN2(TCN2,F,N), A=now(), timer:now_diff(A,B)/N end.
% F = fun() -> ellipticCurves:mult(O-1,P) end.
% G = fun() -> ellipticCurves:pmult(O-1,P) end.
% 
% MM = 10000.
% 
% {TCN(F,MM),TCN(G,MM)}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-module(helper).
-compile({no_auto_import,[min/2,max/2]}).
-export([pow/2,pow/3,mod/2,extended_gcd/2,sqrt/2,min/2,gcd/2,max/2]).
-export([min/1]).
-export([max/1,ceiling/1]).
-export([bitstring_merge/1,after_zero/1]).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      Helper Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% after_zero Removes leading zeros from a bit stream
after_zero(<<0,T/bitstring>>) ->after_zero(T);
after_zero(<<T/bitstring>>)->T;
after_zero(<<>>) -> <<>>.

% bitstring_merge takes a list of bitstreams and concatinates 
%   them together, the reason for this is that the crypto-system 
%   is not bound by word addressable memory blocks, and when 
%   decrypted needs to be placed back into a continuous bit stream.
bitstring_merge(<<>>)-> <<>>;
bitstring_merge([A])-> <<A/bitstring>>;
bitstring_merge([A|T]) -> 
    Rest = bitstring_merge(T),
    <<A/bitstring,Rest/bitstring>>.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Power / Exponent
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Pow uses the 'power' helper function to perform N^E when 
%   applicable, N^E (mod P) is computed in a way that will 
%   minimize computational time, as many of these operations 
%   are very time expensive.
pow(N,E)   -> power(N,E).   % normal  power
pow(N,E,P) -> power(N,E,P). % modulou power

%% fast helper functions for power function
power(N, E) 
  when (is_integer(N) and is_integer(E) and (E >= 0)) -> 
    power(N, E, 1, N).

power(_,0,Acc,_) -> Acc;
power(N, E, Acc, Nsq) when (E rem 2 =:= 1) ->
    power(N, E - 1, Acc * Nsq, Nsq);
power(N, E, Acc, Nsq) ->
    power(N, E div 2, Acc, Nsq * Nsq). 

%% fast helper functions for moludou power function
power(N,E,P) 
  when is_integer(N),
       is_integer(E),
       is_integer(P) ->
    power(N, E, 1, N rem P, P).

power(_,0,Acc,_,_P) -> Acc;
power(N, E, Acc, Nsq,P) when (E rem 2 =:= 1) ->
    power(N, E - 1, ((Acc * Nsq) rem P), Nsq,P);
power(N, E, Acc, Nsq,P) ->
    power(N, E div 2, Acc, ((Nsq * Nsq) rem P),P).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Modulou
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% modulou over Q (the rationals) and over Z (the integers)
%   Modulou over Q is computed using the extended_GCD helper 
%   function that is not exported
mod({Num,Den},Y) when Den > 0->
    {X,_} = extended_gcd(Den,Y),
    mod(X * Num,Y);
mod({Num,Den},Y) when Den < 0->
    {X,_} = extended_gcd(-1*Den,Y),
    mod(X * -1*Num,Y);
mod(X,Y) when X > 0 ->X rem Y;
mod(X,Y) when X < 0 -> Y + X rem Y;
mod(0,_Y) -> 0.

% extended GCD as expected
extended_gcd(_A, B) when B == 0 -> {1,0};
extended_gcd(A, B) ->
    {Q, R} = {A div B,A rem B},
    {S, T} = extended_gcd(B,R),
    {T, S - Q * T}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        sqrt
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% this Integer sqrt requires P to be prime and [P = 3 (mod 4)]
sqrt(I,P) when P rem 4 == 3 ->
    mod(pow(I,(P+1) div 4,P),P).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%             Simple helper functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
min([A])-> A;
min([A|T]) ->min(A,min(T)).

min(P,Q) when P < Q ->P;
min(_P,Q) ->Q.
%%%
max([A])-> A;
max([A|T]) ->max(A,max(T)).

max(P,Q) when P > Q ->P;
max(_P,Q) ->Q.
%%%

gcd(M,N) when M == N -> M;
gcd(M,N) when M > N -> gcd(M-N,N);
gcd(M,N) -> gcd(M,N-M).

% ceilting of x as expected. 
ceiling(X)->
    T = erlang:trunc(X),
    case(X-T) of
	Neg when Neg < 0 ->
	    T;
	Pos when Pos > 0 -> T+1;
	_ ->T
    end.

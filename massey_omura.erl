-module(massey_omura).
-compile({no_auto_import,[min/2]}).
-import(ellipticCurves,[pmult/2,mult/2,makeW/2,makePoint/3,constants/1,value/1]).
-import(helper,[mod/2,pow/2,pow/3,sqrt/2,min/2]).

-export([int2point/3,int2point/4,mess2ints/3,point2int/2,int2mess/3]).
-export([points2mess/4,mess2points/4]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% This moldule supports the massey omura point mapping 
%   algorithm. The helper functions listed here are 
%   used in both sequential and parallel implementations
%   of our cryptographic system. Both directions of 
%   point mapping are supported by this module. The 
%   'to be encrypted' block size is arbitrary and passed 
%   into mess2points at M_Bits

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Converts Messages to Integers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mess2ints(<<>>,List,_M_Bits)->List;
mess2ints(Mess,List,M_Bits)->
    Bits = min(M_Bits,bit_size(Mess)),
    <<Word:Bits/integer,Rem/bitstring>> = Mess,
    mess2ints(Rem,[Word|List],M_Bits).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Makes Numbers Used In Int2Point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
makeX(Max,Mess,J,P)->
    (Max*Mess+J) rem P.
makeS(Xj,A,B,P)->
    (pow(Xj,3)+A*Xj+B) rem P.
makeT(Sj,P)->
    (pow(Sj,(P-1) div 2,P)-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Makes Point from Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
int2point(Mess,Bits,E) ->
    {0,0,0,A,B,P} = constants(E),
    int2point(Mess,pow(2,Bits)-1,A,B,P,E,0).
int2point(Mess,Bits,E,F) ->
    {0,0,0,A,B,P} = constants(E),
    F(int2point(Mess,pow(2,Bits)-1,A,B,P,E,0)).
int2point(Mess,Max,A,B,P,E,J) when J < Max ->
    Xj = makeX(Max,Mess,J,P),
    Sj = makeS(Xj,A,B,P),
    Ts = makeT(Sj,P),

    if 
	(Ts == 0)->
	    Yj = sqrt(Sj,P),
	    makePoint(Xj,Yj,E);%chosen unsafe configuration
	(true) ->
	    int2point(Mess,Max,A,B,P,E,J+1)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Converts a Message Segment into Point
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mess2points(S,M_Bits,E,Func) -> 
    % S denotes the bit stream
    % M_Bits denotes the block size
    % E denotes the field of the Elliptic Curve
    % Func denotes the encryption Function 
    Max = (pow(2,M_Bits)-1),

    Words = lists:reverse(mess2ints(S,[],M_Bits)), 
    %% mess2int can be further optimized

    {0,0,0,A,B,P} = constants(E),
    
    F = fun(W)-> Func(int2point(W,Max,A,B,P,E,0)) end,
    LL = lists:map(F,Words),
    % LL denotes a list of points produced from 
    %   message segments of length M_Bits. 
    LL.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%    DECRYPTION 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Converts a Point to an Integer
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
point2int(P,Max)->
    {X,_} = value(P),
    X div Max.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Converts a Integer into a Message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
int2mess(Int,M_Bits)->
    <<Int:M_Bits/integer>>.
int2mess(Int,M_Bits,0)->
    int2mess(Int,M_Bits);
int2mess(Int,_M_Bits,Offset)->
    <<Int:Offset/integer>>.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%    Converts list of Point s to Message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
points2mess(L,M_Bits,Func,Offset) ->
    % L denotes the List of Points
    % M_Bits denotes the block size
    % Func denotes the decryption Function 
    % Offset is a given amount of space that is different from the 
    %   integer addressable word space
    Max = pow(2,M_Bits)-1,
    
    End = lists:last(L),

    F = fun(W) -> int2mess(point2int(Func(W),Max),M_Bits) end,

    LSTS = lists:map(F,lists:sublist(L,length(L)-1)),
    
    LEND = lists:map(
	     fun(X) -> int2mess(point2int(Func(X),Max),M_Bits,Offset) end,
	     [End]),
    % returns the bit stream as intended
    (LSTS++LEND).


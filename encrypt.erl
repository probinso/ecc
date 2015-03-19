-module(encrypt).

-import(massey_omura,[mess2points/4,points2mess/4,int2point/3,point2int/2]).
-import(ellipticCurves,[makeW/2,mult/2,order/1,curve/1,addPoints/2,inverse/1]).
-import(helper,[sqrt/2,pow/2]).

-export([serverSet/1,getMP/1,key/2,encrypt_file/4,decrypt_file/4,write_to_file/2]).
-export([go/3]).

% the 'encrypt' module implements the sequential ECC system we
%   start with. This however was used as a stepping stone, and now 
%   provides functions used from p_encrypt that support parallel 
%   encryption and decryption. Most of this is depricated, as the 
%   parallel implementation supports N >= 1 threads without 
%   complaint.

% serverSet returns EC field that respects the desired number of 
%   bits. There are restrictions on which primes may be used, 
%   Those restrictions are more clearly defined in my paper, and 
%   Sage Worksheet. 
serverSet(Bits)->
    makeField(Bits).

% go is used for testing Sequential implementation. 
go(Bits,FileIn,FileOut)->
    P = serverSet(Bits),
    {N,NP} = getMP(P),
    {M,MP} = getMP(P),
    FileIn = FileIn,
    FileOut= FileOut,
    
    {ok,B_IN} = file:read_file(FileIn),

    Enc = encrypt_file(B_IN,N,MP,Bits),
    Dec = decrypt_file(Enc,M,NP,Bits),
    Binaries = helper:bitstring_merge(Dec),
    write_to_file(Binaries,FileOut).

% getMP provides MP which we use to generate private and 
%   public key pairs. I know that random:uniform(Val) is 
%   not cryptographically random, but that is not the 
%   focus of this project, and can be appropriatly 
%   addressed when needed. 
% P is a point on the elliptic curve to denote the field
getMP(P)->
    Prime = order(curve(P)),
    Val = sqrt((Prime div 2),Prime),
    M = random:uniform(Val),
    {M,mult(M,P)}.

% key produces a (public / private) key elements
key(M,NP)->
    mult(M,NP).

% encrypt_file takes in the pair N and MP to produce 
%   its encryption key, then sequentially encrypts the 
%   FileIN's data on 'Bits' - size block spaces. Finally 
%   it returns a list of the points. 
% In order to insure behavior as expected, the offset is 
%   stored and encrypted at the head of the list, this 
%   decision is known to the decryption algorithm. 
encrypt_file(FileIN,M,NP,Bits)->
    Key = key(M,NP),
    E = curve(Key),
    F = fun(P)-> addPoints(P,Key) end,
    
    Offset = bit_size(FileIN) rem Bits,
    Head = int2point(Offset,Bits,E),

    MessagePoints = mess2points(FileIN,Bits,E,F),

    [Head]++MessagePoints.

% decrypt_file takes in M and NP to decrypt. The 
%   behavior is not much different from encrypt_file.
decrypt_file(LL,N,MP,Bits)->
    Key = key(N,MP),
    Tmp = inverse(Key),

    Head = hd(LL),
    Offset = point2int(Head,pow(2,Bits)-1),
    
    F = fun(P) -> addPoints(P,Tmp) end,
    points2mess(lists:nthtail(1,LL),Bits,F,Offset).

% writes binary list out to file as string stream. 
write_to_file(BinList,FileOUT)->
    {ok,FStream} = file:open(FileOUT,write),

    List = binary_to_list(BinList),
    io:format(FStream,"~s",[List]),
    
    file:close(FStream).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Accessable Structures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% thses are sample fields, these are used because we don't 
%   want to produce appropriate primes on the fly. 
makeField(X) when (X =< 8) -> 
    {Bits,A,B,P} = 
	{X,14,17,65539},
    field(Bits,A,B,P);
makeField(X) when X =< 15 ->
    {Bits,A,B,P} = {X,14,17,1073741827},
    field(Bits,A,B,P);
makeField(X) when X =< 16 ->
    {Bits,A,B,P} = {X,14,17,4294967311},
    field(Bits,A,B,P);
makeField(X) when X =< 24 ->
    {Bits,A,B,P} = {X,14,17,
		    281474976710731},
    field(Bits,A,B,P);
makeField(X) when X =< 32 ->
    {Bits,A,B,P} = {X,14,17,18446744073709551667},
    field(Bits,A,B,P);
makeField(X) when X =< 33 ->
    {Bits,A,B,P} = {X,14,17,73786976277658337299},
    field(Bits,A,B,P);
makeField(X) when X =< 64->
    {Bits,A,B,P} = {X,14,17,340282366920938463463374607431768211507},
    field(Bits,A,B,P);
makeField(X) when X =< 128->
    {Bits,A,B,P} = {X,14,17,115792089237316195423570985008687907853269984665640564039457584007913129640423},
    field(Bits,A,B,P);
makeField(X) when X =< 192->
    {Bits,A,B,P} = {X,14,17,39402006196394479212279040100143613805079739270465446667948293404245721771497210611414266254884915640806627990307147},
    field(Bits,A,B,P).

field(Bits,A,B,P)->
% I know that random:uniform(Val) is not 
%   cryptographically random, but that is not the 
%   focus of this project, and can be appropriatly 
%   addressed when needed. 
    W = makeW({A,B},P),
    X0 = random:uniform(Bits),
    int2point(X0,Bits,W).

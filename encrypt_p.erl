-module(encrypt_p).
-import(encrypt,[makeFiels/1,key/2]).
-import(ellipticCurves,[curve/1,addPoints/2,inverse/1]).
-import(massey_omura,[points2mess/4,mess2points/4,int2point/4,point2int/2]).
-import(helper,[bitstring_merge/1,ceiling/1,pow/2]).

-export([encrypt_file/5,decrypt_file/5]).
%debug
-export([go/4]).

% the 'encrypt_p' module implements the parallel ECC system we
%   prefer. This uses functions from encrypt that support parallel 
%   encryption and decryption. 

% go is used to debug the process in a usefull manner, it will 
%   be used by the timer module for gathering large bodies of 
%   data on runtime. 
% It is important to note that reading from the file, and 
%   writing to the next file, are kept seperate from this 
%   testing process for timming reasons. The timer will use
%   these tools in a more specific manner. 
go(Threads,Bits,FileIn,FileOut)->
    P = encrypt:serverSet(Bits),
    {N,NP} = encrypt:getMP(P),
    {M,MP} = encrypt:getMP(P),
    FileIn = FileIn,
    FileOut= FileOut,
    
    {ok,B_IN} = file:read_file(FileIn),
    
    Enc = encrypt_file(B_IN,N,MP,Bits,Threads),
    Dec = decrypt_file(Enc,M,NP,Bits,Threads),

    encrypt:write_to_file(Dec,FileOut).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% getJump takes in the number of threads, and the 
%   size of the data to be addressed, and identifies 
%   how much data should be processed by each thread.
getJump(List,Threads,_Bits) when is_list(List)->
    ceiling(length(List)/Threads);
getJump(FileIn,Threads,Bits)->
    Tmp = ceiling(bit_size(FileIn)/Threads),
    Tmp - (Tmp rem Bits).

% encrypt_file takes in the pair N and MP to produce 
%   its encryption key, then encrypts the FileIN's 
%   data on 'Bits' - size block spaces. Finally 
%   it returns a list of the points. 
% In order to insure behavior as expected, the offset is 
%   stored and encrypted at the head of the list, this 
%   decision is known to the decryption algorithm. 
encrypt_file(FileIN,N,MP,Bits,Threads)->
    Key = key(N,MP),
    E = curve(Key),
    
    F = fun(P)-> addPoints(P,Key) end,
    G = fun(Seg)-> mess2points(Seg,Bits,E,F) end,
    
    Offset = bit_size(FileIN) rem Bits,
    Head = int2point(Offset,Bits,E,F),
    
    Jump = getJump(FileIN,Threads,Bits),
    
    Self = self(),
    Ref = erlang:make_ref(),
    
    Pids = lists:reverse(segment_process(Self,Ref,encrypt,FileIN,Jump,G,[])),
    ([Head]++lists:flatten(gather(Pids,Ref))).

% decrypt_file takes in M and NP to decrypt. The 
%   behavior is not much different from encrypt_file.
decrypt_file(LL,M,NP,Bits,Threads)->
    Key = key(M,NP),
    Tmp = inverse(Key),

    F = fun(P) -> addPoints(P,Tmp) end,
    G = fun(List,I) -> bitstring_merge(points2mess(List,Bits,F,I)) end,

    
    Head = hd(LL),
    Offset = point2int(F(Head),helper:pow(2,Bits)-1),

    Jump = getJump(tl(LL),Threads,Bits),

    Self = self(),
    Ref = erlang:make_ref(),
    
    Pids = lists:reverse(segment_process(
                                        Self,
                                        Ref,
                                        decrypt,
                                        tl(LL),
                                        Jump,
                                        G,
                                        [],
                                        Offset)),
    bitstring_merge(gather(Pids,Ref)).

% is a helper function that enables collection of data from spawned 
%   threads in an expected order. 
gather([Pid|T],Ref)->
    receive {Pid,Ref,Ret} 
      -> [Ret|gather(T,Ref)] 
    end;
gather([],_) -> [].
 
% segment process appropriately spawns threads baised on imput functions
%   because this is rather messy, the message is kept succinct. 
% 
% the code below is indended for 'encrypt'
segment_process(_Parent,_Ref,encrypt,<<>>,_Jump,_Func,L)->L;
segment_process(Parent,Ref,encrypt,BinaryList,Jump,Func,L)->
    %List is a Binary representation of the data
    MYJMP = helper:min(Jump,bit_size(BinaryList)),

    <<Start:MYJMP/bitstring,Rem/bitstring>> = BinaryList,
    SP = fun() -> Parent ! {self(),Ref,Func(Start)} end,
    Pid = spawn(SP),

    segment_process(Parent,Ref,encrypt,Rem,Jump,Func,[Pid|L]).
    
% segment process appropriately spawns threads baised on imput functions
%   because this is rather messy, the message is kept succinct.
%
% the code below is intended for 'decrypt'  
segment_process(_Parent,_Ref,decrypt,[],_Jump,_Func,L,_Offset)->L;
segment_process(Parent,Ref,decrypt,PointList,Jump,Func,L,Offset) when length(PointList) =< Jump ->
    MYJMP = helper:min(Jump,length(PointList)),
    {Start,Rem} = lists:split(MYJMP,PointList),
    SP = fun() -> Parent ! {self(),Ref,Func(Start,Offset)} end,
    Pid = spawn(SP),
    segment_process(Parent,Ref,decrypt,Rem,Jump,Func,[Pid|L],Offset);
segment_process(Parent,Ref,decrypt,PointList,Jump,Func,L,Offset)->
    MYJMP = helper:min(Jump,length(PointList)),
    {Start,Rem} = lists:split(MYJMP,PointList),
    SP = fun() -> Parent ! {self(),Ref,Func(Start,0)} end,
    Pid = spawn(SP),
    segment_process(Parent,Ref,decrypt,Rem,Jump,Func,[Pid|L],Offset).

-module(ellipticCurves).
%pointOps
-export([inverse/1,makePoint/3,addPoints/2,mult/2,pmult/2,infPoint/1,value/1,curve/1]).
-export([print/1,makeW/2,makeW/1,order/1,constants/1]).

-import(helper,[pow/2,mod/2,extended_gcd/2]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                        Record Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(weirstrass,{
%y^2 + a_1 xy + a_3 y = x^3 + a_2 x^2 + a_4 x + a_6.
	  a1 = 1,
	  a2 = 1,
	  a3 = 0,
	  a4 = 0,
	  a6 = 1,
	  mod= 101
	 }).

-record(point, {
	  x = 0,
	  y = 0,
	  curve = #weirstrass{}
	 }).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Curve Creation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
discriminant(A1,A2,A3,A4,A6)-> 
    % used sage to compute this
    %   The discriminant of an elliptic curve is only used to 
    %   insure the curve you are working with is valid.
    %   
    %   this is a mess of an equasion, but it has been checked. 
    9*(pow(A3,2) + 4*A6)*(A1*A3 + 2*A4)*(pow(A1,2) + 4*A2) - 
      pow(27*(pow(A3,2) + 4*A6),2) - 8*pow(A1*A3 + 2*A4,3) - 
      pow(pow(A1,2) + 4*A2,2) * 
      (pow(A1,2)*A6 - A1*A3*A4 + A2*pow(A3,2)+4*A2*A6 - pow(A4,2)).

% makeW returns a curve in Weirstrass Form. It would be nice to 
%   explore other forms, as many are faster for point multiplcation
%   
%   Reduced Weirstrass form can be used for most curve-field pairs
%   Given only N (the size of our field) we use a default curve as  
%   listed above 
makeW({A4,A6},N)->makeW({0,0,0,A4,A6},N);
makeW({A1,A2,A3,A4,A6},N)->
    D = discriminant(A1,A2,A3,A4,A6),
    if (D /= 0) -> #weirstrass{a1=A1,a2=A2,a3=A3,a4=A4,a6=A6,mod=N};
	  true -> {error, "Curve Discriminant == 0"}
    end.
makeW(N) when is_integer(N)->
    #weirstrass{mod=N};
makeW(_)->
    #weirstrass{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    Curve Operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
order(E) when is_record(E,weirstrass)->
    #weirstrass{_=_,mod=N}=E,
    N;
order(P) when is_record(P,point)->
    % this is a stub. As it is the case that point order is 
    %   unnessicary for this cryptography project, it will be 
    %   omitted, from results. 
    P.

curve(P) ->
    % given point return curve. 
    #point{_=_,curve=E} = P,
    E.

constants(E) when is_record(E,weirstrass)->
    % given Curve return Constants
    #weirstrass{a1=A1,a2=A2,a3=A3,a4=A4,a6=A6,mod=N} = E,
    {A1,A2,A3,A4,A6,N};
    
constants(P) when is_record(P,point)->
    % given point return constants of respective curve
    #point{_=_,curve=E} = P,
    #weirstrass{a1=A1,a2=A2,a3=A3,a4=A4,a6=A6,mod=N} = E,
    {A1,A2,A3,A4,A6,N}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Point Operations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
value(P)->
    % given point return X,Y tupple
    #point{x=X,y=Y,curve=_E} = P,
    {X,Y}.

makePoint(X,Y,{E,safe}) when is_record(E,weirstrass) ->
    % this is safe make. It verifys that the points produced 
    %   are on the curve as expected. It turns out that this 
    %   is primarily usefull for testing purposes. 
    #weirstrass{a1=A1,a2=A2,a3=A3,a4=A4,a6=A6,mod=N}=E,
    S = pow(Y,2)+A1*X*Y+A3*Y,
    T = pow(X,3)+A2*pow(X,2)+A4*X+A6,
    P = ((T-S) rem N),
    if 0 == P 
      -> #point{x=X,y=Y,curve=E};
    true 
      -> {error,{X,Y},notOnCurve,P}
    end;
    
makePoint(X,Y,{E,mod}) when is_record(E,weirstrass) ->
    % this is mod make. It takes an X,Y pair and makes the 
    %   claim that it is on curve E. It does not verify results
    #weirstrass{_=_,mod=N}=E,
    makePoint(mod(X,N),mod(Y,N),E);
    
makePoint(X,Y,{E,mod,safe}) when is_record(E,weirstrass) ->
    % this is mod make. with safety
    #weirstrass{_=_,mod=N}=E,
    makePoint(mod(X,N),mod(Y,N),{E,safe});
    
makePoint(X,Y,E) when is_record(E,weirstrass) ->
    % this is unsafe make. This is our fast point making 
    %   operation that will be used in the final product
    #point{x=X,y=Y,curve=E}.

% the inverse of a point P, X is the point such that P+X = inf 
% which is The identity element
inverse(P) when (P#point.x=="oo") -> P;
inverse(P) ->
    %Lawrence C. Washington, Elliptic Curves Page 15.
    #point{x=Px,y=Py, curve=#weirstrass{a1=A1,a2=_,a3=A3,a4=_,a6=_,mod=N}}=P,
    P#point{y=mod((-A1*Px -A3-Py),N)}. 

% infPoint produces infinity (or identity) elements along our curve
infPoint(E) when is_record(E,weirstrass) 
  -> #point{x="oo",y="oo",curve=E};
infPoint(P) when is_record(P,point) 
  ->#point{x="oo",y="oo",curve=P#point.curve}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                   Point Addition 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
newSlope(P0,P1) when (P0 == P1)->
    % newSlope is a helper function to allow for easier point 
    %   addition. When the points are equivelent, the tangent 
    %   of the curve at the point is our desired slope 
    #point{x=Px0,y=Py0,curve=E} = P0,
    #weirstrass{a1=A1,a2=A2,a3=A3,a4=A4,a6=_A6,mod=N} = E,
    Num=mod(3*pow(Px0,2)+2*A2*Px0+A4-A1*Py0,N),
    Den=mod(2*Py0+A1*Px0+A3,N),
    mod({Num,Den},N);
    
newSlope(P0,P1) ->
    % when the points are not equal, we use the point difference 
    %   to accuire our desired slope value. 
    #point{ 
      x=Px0,
      y=Py0,
      curve=#weirstrass{a1=_A1,a2=_A2,a3=_A3,a4=_A4,a6=_A6,mod=N}} = P0,
    % we only unpack from P0 because we assume that both points 
    %   are on the same curve E. We are alson only concerned 
    %   with the moduous of the field. 
    #point{x=Px1,y=Py1,curve=_E}= P1,
    Num = (Py1-Py0),
    Den = (Px1-Px0),
    mod({Num,Den},N).

% Addition of points over an elliptic curve is computed by taking 
%   any two points, drawing a line connecting the first to the 
%   second, and continuing that line until a third intersection 
%   point is made. The Inverse of the third intersection point 
%   is the sum of the first two points. 
addPoints(P0,P1) when (P0#point.x=="oo"),
                      (P1#point.x=="oo") -> P1;

addPoints(P0,P1) when (P0#point.x=="oo") -> addPoints(P1,P0);

addPoints(P0,P1) when (P1#point.x=="oo") -> P0;

addPoints(P0,P1) when (P0#point.x==P1#point.x),
                      (P0#point.y/=P1#point.y) -> infPoint(P0);
addPoints(P0,P1) ->
    #point{
      x=Px0,
      y=Py0,
      curve=#weirstrass{a1=A1,a2=A2,a3=_A3,a4=_A4,a6=_A6,mod=N}} = P0,
    % we again assume that the two points are on the same curve
    #point{x=Px1,y=_Py1,curve=E}= P1,
    
    M = newSlope(P0,P1),
    
    X=mod(A1*M+pow(M,2)-A2 -Px0-Px1,N), 
    Y=mod(M*X - M*Px0+Py0,N),
    
    %inverse(makePoint(X,Y,{E,safe})). % -- safe -- %
    %  we abandon safe implementation because we are done debugging
    inverse(makePoint(X,Y,E)).


%%%%-------------------------------------------------------------
%%                      Integer * Point
%%%%-------------------------------------------------------------
smult({A,B,_C}) when (A == 0) -> B;
smult({A,B,C}) when (A rem 2 == 0) -> smult({A div 2, B,addPoints(C,C)});
smult({A,B,C}) -> smult({A-1,addPoints(B,C),C}).

% mult multiplies an integer by a point by the use of the more 
%   traditional point doubling method. This uses the smult helper 
%   function which is not exported
mult(I,P) -> 
    #point{x=_,y=_,curve=#weirstrass{a1=_,a2=_,a3=_,a4=_,a6=_,mod=N}}= P,
    K = mod(I,N),
    smult({K,infPoint(P),P}).

% fibPoint is a thought exersize, The questions was could we add 
%   points faster, if we used fibbonaci point accumulation rather 
%   than point doubling. This has proven less than interesting. 
fibPoint({P,_Q,N,_M,I}) when 2*N > I -> {P,I-N};
fibPoint({P,Q,N,M,I})->
    {R,S} = fibPoint({addPoints(P,Q),P,N+M,N,I}),
    if
	(M =< S) ->
	    {addPoints(R,Q),S-M};
	(true) ->
	    {R,S}
    end.

% pmult multiplies a point by an integer using the fibPoint algorithm.
%   this is deprecated. 
pmult(I,P) -> 
    #point{_=_,curve=#weirstrass{_=_,mod=N}}= P,
    K = mod(I,N),
    {Q,_} = fibPoint({P,infPoint(P),1,0,K}),
    Q.

%%%%-------------------------------------------------------------
%%                Print -- Used in debugging 
%%%%-------------------------------------------------------------
print(E) when is_record(E,weirstrass)->
    #weirstrass{a1=A1,a2=A2,a3=A3,a4=A4,a6=A6,mod=N} = E,
    lists:concat(["y^2 + ",A1,"*x*y + ",A3,"*y = x^3 + ",A2,"*x^2 + ",A4,"*x + ",A6,"   over Z(",N,")."]);
print(P) when is_record(P,point)->
    #point{x=Px,y=Py,curve=E} = P,
    lists:concat(["( ",Px," , ",Py," ) ~n on ",print(E)]).


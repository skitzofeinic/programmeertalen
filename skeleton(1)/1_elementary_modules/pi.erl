-module (pi).
-export ([pi/0]).

% ============================ %
%  Calls the main pi function. %
% ============================ %

pi() -> calc(1,3,-1).

% =================================================== %
% A - current value in brackets (1 - 1/3 + 1/5 - ...)
% B - denominator
% C - sign(+/-)
%
% Recursive function that calculates pi.
% =================================================== %
calc(A,B,C) -> 0.
%%%%%%
%%%%%%   Your erlang code here
%%%%%

